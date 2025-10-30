# Description
Side project for fun: Emulating a 6502 CPU in Rust.

Not (currently) implementing any of the "illegal" opcodes, but we'll see.

One extra opcode (0xFF => HLT) is provided which is simply used to cleanly break out of the emulation loop.

Can direct CPU behaviour when decoding the HLT opcode specifically, as well as the other illegal opcodes:
- If the`CPU6502`'s `allow_hlt` field is set to `true` (default), the HLT opcode will always be recognized by the CPU and will not cause a panic. If it is set to `false`, the HLT opcode will be treated like any other illegal opcode, whose behaviour is deterined by the `illegal_opcode_mode` field.
- If the `CPU6502`'s `illegal_opcode_mode` field is set to `true` (default), illegal opcodes will be decoded into NOP instructions. If it is set to `false`, illegal opcodes will cause the CPU to panic.

# Documentation and information sources:
- http://www.6502.org/users/obelisk/6502/index.html
- [NESDEV wiki - CPU](https://www.nesdev.org/wiki/CPU)
  - [CPU specifications, TXT](https://www.nesdev.org/6502_cpu.txt)
  - [Status flags](https://www.nesdev.org/wiki/Status_flags#C)
- [Interactive Instruction Set documentation](https://www.masswerk.at/6502/6502_instruction_set.html)
- [Writing your own NES emulator Part 3 - the 6502 CPU](https://yizhang82.dev/nes-emu-cpu)