package org.github.hyunsik.grammar;

import com.github.hyunsik.grammar.LangLexer;
import com.github.hyunsik.grammar.LangParser;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;

public class Parser {
  public static LangParser.Package_contentsContext parse(String sourceCode) {
    final ANTLRInputStream input = new ANTLRInputStream(sourceCode);
    final LangLexer lexer = new LangLexer(input);
    lexer.removeErrorListeners();
    lexer.addErrorListener(new LangErrorListener());

    final CommonTokenStream tokens = new CommonTokenStream(lexer);


    final LangParser parser = new LangParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(new LangErrorListener());
    parser.setBuildParseTree(true);

    return parser.package_contents();
  }
}
