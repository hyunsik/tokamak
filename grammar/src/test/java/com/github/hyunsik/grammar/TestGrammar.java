package com.github.hyunsik.grammar;

import org.github.hyunsik.grammar.Parser;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

public class TestGrammar {

  @Rule public TestName name = new TestName();

  public Optional<Collection<File>> getResourceFiles(String subdir) throws URISyntaxException, IOException {
    URL uri = ClassLoader.getSystemResource(subdir);
    if (uri == null) {
      return Optional.empty();
    } else {
      Path dir = FileSystems.getDefault().getPath(uri.getPath());

      return Optional.of(StreamSupport.stream(Files.newDirectoryStream(dir).spliterator(), false)
          .map(p -> new File(p.toUri()))
          .collect(Collectors.toList()));
    }
  }

  public static String readTextFile(File file) {
    StringBuilder fileData = new StringBuilder(1000);
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader(file));
    } catch (FileNotFoundException e) {
      throw new RuntimeException(e);
    }

    char[] buf = new char[1024];
    int numRead;
    try {
      while ((numRead = reader.read(buf)) != -1) {
        String readData = String.valueOf(buf, 0, numRead);
        fileData.append(readData);
        buf = new char[1024];
      }
    } catch (IOException ioe) {
      throw new RuntimeException(ioe);
    } finally {
      try {
        reader.close();
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
    return fileData.toString();
  }

  /**
   * Return a pair of file name and SQL query
   *
   * @return a pair of file name and SQL query
   * @throws IOException
   * @throws URISyntaxException
   */
  public Optional<Collection<Pair<String, String>>> getFileContents(String subdir) throws IOException, URISyntaxException {
    Optional<Collection<File>> files = getResourceFiles(subdir);

    if (files.isPresent()) {
      return Optional.of(files.get().stream()
          .map(file -> new Pair<>(file.getName(), readTextFile(file)))
          .collect(Collectors.toList()));
    } else {
      return Optional.empty();
    }
  }

  public void verifySuccessAndFail() throws IOException, URISyntaxException {
    Optional<Collection<Pair<String, String>>> s = getFileContents(name.getMethodName() + "/success");
    if (s.isPresent()) {
      for (Pair<String, String> source : s.get()) {
        try {
          assertNotNull(Parser.parse(source.getSecond()));
          System.out.println(source.getFirst() + " test passed..");
        } catch (Throwable t) {
          fail("Positive test '" + source.getFirst() + "' failed..\n" + t.getMessage());
        }
      }
    }

    Optional<Collection<Pair<String, String>>> f = getFileContents(name.getMethodName() + "/fail");
    if (f.isPresent()) {
      for (Pair<String, String> source : f.get()) {
        try {
          assertNotNull(Parser.parse(source.getSecond()));
          fail();
        } catch (AssertionError ae) {
          fail("Negative test '" + source.getFirst() + "' failed..\n");
        } catch (Throwable t) {
          System.out.println(source.getFirst() + " test passed..");
        }
      }
    }
  }

  @Test
  public void func() throws IOException, URISyntaxException {
    verifySuccessAndFail();
  }
}
