#!/bin/bash

python ./generator.py -i x86/info.json --format compiler-defs x86/avx.json x86/avx2.json x86/sse.json x86/sse2.json x86/sse3.json x86/sse41.json x86/sse42.json x86/ssse3.json -o ../../src/platform-intrinsics/x86.rs
python ./generator.py --format compiler-defs arm.json -o ../../src/platform-intrinsics/arm.rs
python ./generator.py --format compiler-defs aarch64.json -o ../../src/platform-intrinsics/aarch64.rs
