package org.github.hyunsik.grammar;

import org.antlr.v4.runtime.*;
import org.apache.commons.lang.StringUtils;

public class LangErrorListener extends BaseErrorListener {

  public void syntaxError(Recognizer<?, ?> recognizer,
                          Object offendingSymbol,
                          int line, int charPositionInLine,
                          String msg,
                          RecognitionException e) {
    CommonTokenStream tokens = (CommonTokenStream) recognizer.getInputStream();
    String input = tokens.getTokenSource().getInputStream().toString();
    Token token = (Token) offendingSymbol;
    String[] lines = StringUtils.splitPreserveAllTokens(input, '\n');
    String errorLine = lines[line - 1];

    String simpleMessage = "syntax error at or near \"" + token.getText() + "\"";
    throw new LangParserException(token, line, charPositionInLine, simpleMessage, errorLine);
  }
}
