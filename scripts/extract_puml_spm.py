#!/usr/bin/env python3
"""Extract PlantUML source files from a puml.spm archive.

The puml.spm format is a Java DataOutputStream solid archive:

    int32      entry_count
    for each entry:
        utf      key            # relative path without .puml extension
        int32    content_length
        bytes    content        # UTF-8 encoded PlantUML source

Files found under `output/` are Brotli-compressed by the encoder; files under
`raw/` are uncompressed. This script tries Brotli decompression first and
falls back to reading the file as raw archive data.
"""

import argparse
import sys
from io import BytesIO
from pathlib import Path
from typing import BinaryIO

try:
    import brotli
except ImportError as e:  # pragma: no cover
    raise SystemExit(
        "The 'brotli' package is required. "
        "Install it with: uv pip install -r requirements.txt"
    ) from e


def read_int32(f: BinaryIO) -> int:
    """Read a big-endian signed 32-bit integer (Java DataOutputStream)."""
    data = f.read(4)
    if len(data) != 4:
        raise EOFError("Unexpected end of file while reading int32")
    return int.from_bytes(data, byteorder="big", signed=True)


def read_utf(f: BinaryIO) -> str:
    """Read a Java DataOutputStream UTF-8 string.

    The string is prefixed with a big-endian unsigned 16-bit length, followed by
    that many modified UTF-8 bytes. For the keys and content used by PlantUML
    stdlib archives, standard UTF-8 decoding is sufficient.
    """
    length_bytes = f.read(2)
    if len(length_bytes) != 2:
        raise EOFError("Unexpected end of file while reading UTF length")
    length = int.from_bytes(length_bytes, byteorder="big", signed=False)
    data = f.read(length)
    if len(data) != length:
        raise EOFError(f"Expected {length} UTF bytes, got {len(data)}")
    return data.decode("utf-8", errors="replace")


def decompress(data: bytes) -> bytes:
    """Try Brotli decompression; return raw data if it fails."""
    try:
        return brotli.decompress(data)
    except brotli.error:
        return data


def extract_puml_spm(spm_path: Path, out_dir: Path) -> int:
    """Extract all entries from a single puml.spm file into out_dir."""
    data = decompress(spm_path.read_bytes())

    extracted = 0
    stream = BytesIO(data)
    count = read_int32(stream)

    for _ in range(count):
        key = read_utf(stream)
        length = read_int32(stream)
        content = stream.read(length)
        if len(content) != length:
            raise EOFError(
                f"Expected {length} content bytes for key '{key}', got {len(content)}"
            )

        # The key stored in the archive does not include the .puml extension.
        rel_path = key + ".puml"
        dest = out_dir / rel_path
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(content)
        extracted += 1

    return extracted


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Extract PlantUML sources from puml.spm archives."
    )
    parser.add_argument(
        "input_dir",
        type=Path,
        help="Directory to search for puml.spm files (e.g. output/stdlib).",
    )
    parser.add_argument(
        "output_dir",
        type=Path,
        help="Directory where extracted .puml files will be written.",
    )
    args = parser.parse_args()

    if not args.input_dir.is_dir():
        print(f"Error: input directory does not exist: {args.input_dir}", file=sys.stderr)
        return 1

    args.output_dir.mkdir(parents=True, exist_ok=True)

    spm_files = sorted(args.input_dir.rglob("puml.spm"))
    if not spm_files:
        print(f"No puml.spm files found under {args.input_dir}", file=sys.stderr)
        return 1

    total_files = 0
    total_entries = 0

    for spm_path in spm_files:
        # Preserve the library name from the parent directory.
        lib_dir = spm_path.parent
        out_lib_dir = args.output_dir / lib_dir.name
        try:
            entries = extract_puml_spm(spm_path, out_lib_dir)
            total_files += 1
            total_entries += entries
            print(f"Extracted {entries} entries from {spm_path} -> {out_lib_dir}")
        except Exception as exc:  # pragma: no cover
            print(f"Error extracting {spm_path}: {exc}", file=sys.stderr)
            return 1

    print(f"Done: {total_entries} entries from {total_files} puml.spm files.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
