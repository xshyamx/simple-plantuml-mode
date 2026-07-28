import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;

import net.sourceforge.plantuml.brotli.BrotliInputStream;

/**
 * Extract PlantUML source files from puml.spm archives.
 *
 * <p>The puml.spm format is a Java DataOutputStream solid archive:
 * <pre>
 *     int32      entry_count
 *     for each entry:
 *         utf      key            // relative path without .puml extension
 *         int32    content_length
 *         bytes    content        // UTF-8 encoded PlantUML source
 * </pre>
 *
 * <p>Files may be Brotli-compressed. This class tries Brotli decompression
 * first and falls back to reading the file as raw archive data.
 */
public class ExtractPumlSpm {

    /**
     * Try Brotli decompression; return raw data if it fails.
     */
    private static byte[] decompress(byte[] data) {
        try (BrotliInputStream brotli = new BrotliInputStream(new ByteArrayInputStream(data))) {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] buf = new byte[8192];
            int n;
            while ((n = brotli.read(buf)) != -1) {
                out.write(buf, 0, n);
            }
            return out.toByteArray();
        } catch (IOException e) {
            // Not Brotli-compressed, return raw
            return data;
        }
    }

    /**
     * Extract all entries from a single puml.spm file into outDir.
     *
     * @return the number of entries extracted
     */
    private static int extractPumlSpm(Path spmPath, Path outDir) throws IOException {
        byte[] raw = Files.readAllBytes(spmPath);
        byte[] data = decompress(raw);

        DataInputStream stream = new DataInputStream(new ByteArrayInputStream(data));
        int count = stream.readInt();
        int extracted = 0;

        for (int i = 0; i < count; i++) {
            String key = stream.readUTF();
            int length = stream.readInt();
            byte[] content = new byte[length];
            stream.readFully(content);

            // The key stored in the archive does not include the .puml extension.
            Path dest = outDir.resolve(key + ".puml");
            Files.createDirectories(dest.getParent());
            Files.write(dest, content);
            extracted++;
        }

        return extracted;
    }

    /**
     * Recursively find all files named "puml.spm" under a directory.
     */
    private static List<Path> findSpmFiles(Path inputDir) throws IOException {
        List<Path> result = new ArrayList<>();
        Files.walkFileTree(inputDir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                if (file.getFileName().toString().equals("puml.spm")) {
                    result.add(file);
                }
                return FileVisitResult.CONTINUE;
            }
        });
        Collections.sort(result);
        return result;
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: ExtractPumlSpm <input_dir> <output_dir>");
            System.err.println();
            System.err.println("Extract PlantUML sources from puml.spm archives.");
            System.err.println("  input_dir   Directory to search for puml.spm files (e.g. stdlib/).");
            System.err.println("  output_dir  Directory where extracted .puml files will be written.");
            System.exit(1);
        }

        Path inputDir = Paths.get(args[0]);
        Path outputDir = Paths.get(args[1]);

        if (!Files.isDirectory(inputDir)) {
            System.err.println("Error: input directory does not exist: " + inputDir);
            System.exit(1);
        }

        try {
            Files.createDirectories(outputDir);

            List<Path> spmFiles = findSpmFiles(inputDir);
            if (spmFiles.isEmpty()) {
                System.err.println("No puml.spm files found under " + inputDir);
                System.exit(1);
            }

            int totalFiles = 0;
            int totalEntries = 0;

            for (Path spmPath : spmFiles) {
                // Preserve the library name from the parent directory.
                Path libDir = spmPath.getParent();
                Path outLibDir = outputDir.resolve(libDir.getFileName().toString());

                int entries = extractPumlSpm(spmPath, outLibDir);
                totalFiles++;
                totalEntries += entries;
                System.out.println("Extracted " + entries + " entries from " + spmPath + " -> " + outLibDir);
            }

            System.out.println("Done: " + totalEntries + " entries from " + totalFiles + " puml.spm files.");
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }
}
