package com.github.hyunsik.grammar;

import org.github.hyunsik.grammar.Parser;
import org.junit.Test;

import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

public class TestGrammar {

  public Collection<File> getResourceFiles(String subdir) throws URISyntaxException, IOException {
    URL uri = ClassLoader.getSystemResource(subdir);
    Path dir = FileSystems.getDefault().getPath(uri.getPath());

    return StreamSupport.stream(Files.newDirectoryStream(dir).spliterator(), false)
        .map(p -> new File(p.toUri()))
        .collect(Collectors.toList());
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
  public Collection<Pair<String, String>> getFileContents(String subdir) throws IOException, URISyntaxException {
    return getResourceFiles(subdir).stream()
        .map(file -> new Pair<>(file.getName(), readTextFile(file)))
        .collect(Collectors.toList());
  }

  @Test
  public void testOne() throws IOException, URISyntaxException {
    for (Pair<String, String> source : getFileContents("success")) {
      try {
        assertNotNull(Parser.parse(source.getSecond()));
        System.out.println(source.getFirst() + " test passed..");
      } catch (Throwable t) {
        fail(source.getFirst() + " test failed..\n" +t.getMessage());
      }
    }
  }
}
