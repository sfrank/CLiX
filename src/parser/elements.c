/* This is an interface program that uses expat to parse XML and
 * output a Lispified representation that can be easily parsed by the
 * normal Common Lisp reader.
 */

#include <stdio.h>
#include "xmlparse.h"

/* Since XML PCDATA elements can be returned in multiple chunks by
 * expat, and we want this merged into one string for the Lisp side of
 * things, we keep track of the current inText state, i.e. whether the
 * last thing we output was text, or not.  We only write opening
 * double quotes on !inText -> inText transitions, and closing douple
 * quotes on inText -> !inText transitions.
 */

void finishText(int *inText)
{
  if (*inText)
  {
      putchar('"');
      putchar('\n');
      *inText=0;
  }
} 

void startText(int *inText)
{
    if (!(*inText))
    {
	putchar('"');
	*inText=1;
    }
}

/* Handle conversion from UTF-8 to ISO Latin-1, to which we restrict
 * our Lisp side support for the moment.  Characters outside of the
 * ISO Latin-1 8bit range will be SILENTLY elided. */

void outputText(const unsigned char* text,int len)
{
  int pos=0;
  
  while (pos<len)
    {
      if (text[pos] < 0x80) 
	{
	  /* ASCII:  Output verbatim, except for escape-chars */
	  if (text[pos] == '\\' || text[pos] == '"')
	    putchar('\\');
	  putchar(text[pos++]);
	}
      else if (text[pos] < 0xC0) 
	{
	  /* We are in the middle of a multi-byte sequence!
	   * This should never happen, so we skip it. */
	  pos++;
	}
      else if (text[pos] < 0xE0) 
	{
	  /* Two-byte sequence.  Skip if follow on char is not a
	   * valid continuation byte: */
	  if ((pos+1>=len) || ((text[pos+1] & 0x80) != 0x80))
	    {
	      pos++;
	      continue;
	    }
	  /* Check whether we have a valid ISO Latin-1 character: */
	  if (text[pos] < 0xC4) 
	    {
	      /* Valid, output this and next byte */
	      putchar(((text[pos] & 0x03) << 6) | (text[pos+1] & 0x3f));
	    }
	  
	  pos+=2;
	}
      else if (*text < 0xF0)
	{
	  /* Three-byte sequence. Skip it. */
	  if ((pos+1>=len) || ((text[pos+1] & 0x80) != 0x80))
	    {
	      pos++;
	      continue;
	    }
	  if ((pos+2>=len) || ((text[pos+2] & 0x80) != 0x80))
	    {
	      pos+=2;
	      continue;
	    }
	  pos+=3;
	}
      else
	{
	  /* 4 to 6 byte sequences can't happen in XML, which only
	   * uses the BMP, aka Unicode.  We skip until the next non
	   * continuation character. */
	  do 
	    {
	      pos++;
	    }
	  while ((pos<len) && ((text[pos] & 0x80) == 0x80));
	}
    }
}
	  
void outputString(const unsigned char* text)
{
  outputText(text,strlen(text));
}

/* Handle Element start and stop tags */

void startElement(void *userData, const char *name, const char **atts)
{
  const char** att;
  finishText((int*)userData);
  fputs("((\"",stdout);
  outputString(name);
  putchar('"');
  for (att=atts;*att;att+=2)
    {
      fputs(" \"",stdout);
      outputString(*att);
      fputs("\" \"",stdout);
      outputString(*(att+1));
      fputs("\"",stdout);
    }
  fputs(")\n",stdout);
}

void endElement(void *userData, const char *name)
{
    finishText((int*)userData);
    fputs(")\n",stdout);
}

/* Handle PCDATA */

void charData(void* userData, const XML_Char *s,int len)
{
    int i;
    startText((int*)userData);
    outputText(s,len);
}

/* Handle PIs */
void processingInstruction(void* userData,const XML_Char *target,const XML_Char *data)
{
  finishText((int*)userData);
  fputs("(\"",stdout);
  outputString(target);
  fputs("\" . \"",stdout);
  outputString(data);
  fputs("\")",stdout);
}

/* Main program */

int main()
{
  char buf[BUFSIZ];
#ifndef CL_NS_SEP
  XML_Parser parser = XML_ParserCreate(NULL);
#else
  XML_Parser parser = XML_ParserCreateNS(NULL,CL_NS_SEP);
#endif
  int done;
  int inText = 0;
  XML_SetUserData(parser, &inText);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, charData);
  XML_SetProcessingInstructionHandler(parser,processingInstruction);
  do {
    size_t len = fread(buf, 1, sizeof(buf), stdin);
    done = len < sizeof(buf);
    if (!XML_Parse(parser, buf, len, done)) {
      fprintf(stderr,
	      "%s at line %d\n",
	      XML_ErrorString(XML_GetErrorCode(parser)),
	      XML_GetCurrentLineNumber(parser));
      return 1;
    }
  } while (!done);
  XML_ParserFree(parser);
  return 0;
}
