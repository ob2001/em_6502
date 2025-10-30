use crate::prelude::*;

pub type Mem = [CPUByte; CPU_MEMSIZE];
pub type MemResult = Result<Mem, &'static str>;

/// Create new cpu memory with all bytes set to the provided value
pub fn new_all(b: CPUByte) -> Mem {
    [b; CPU_MEMSIZE]
}

/// Create new cpu memory with all NOP instructions
pub fn new_nops() -> Mem {
    new_all(0x00)
}

/// Create new cpu memory with all (illegal) HLT instructions
pub fn new_hlts() -> Mem {
    new_all(0xFF)
}

/// Return new zeroed cpu memory with `code` inserted in range `mem[idx]` - `mem[idx + code.len()`.
pub fn new_nops_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    } else {
        let mut ret = new_nops();

        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }

        Ok(ret)
    }
}

pub fn new_hlts_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    } else {
        let mut ret = new_hlts();

        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }

        Ok(ret)
    }
}

/// First iteration of a function to generate CPU memory from a file specification
pub fn new_from_file(file: String) -> MemResult {
    if let Ok(f) = std::fs::read_to_string(file) {
        let mut ret = new_nops();
        let mut radix_mode: u32 = 16;

        for line in f.lines() {
            let trim_line = line.trim();

            if trim_line.len() == 0 {
                continue;
            }

            match trim_line.as_bytes()[0] as char {
                ';' => continue,
                _ => {},
            };

            let (pre, post) = trim_line.split_once(':').unwrap();            
            let mut post = post.split(',');

            match pre {
                "pad_all" => {
                    ret = new_all(u8::from_str_radix(post.next().unwrap().trim(), 16).unwrap_or_else(|_| panic!("Invalid value for padding bytes")));
                },
                "radix_mode" => {
                    radix_mode = post.next().unwrap().trim().parse::<u32>().unwrap_or_else(|_| panic!("Invalid radix mode"));
                },
                a => {
                    let addr = u16::from_str_radix(a.trim(), radix_mode).unwrap_or_else(|_| panic!("Invalid address predicate: {}", pre));
                    let mut mem_line: Vec<u8> = Vec::new();
                    for b in post {
                        mem_line.push(u8::from_str_radix(b.trim(), radix_mode).unwrap_or_else(|_| panic!("Invalid number representation: {} for radix mode {}", b, radix_mode)));
                    }

                    insert_code_at(&mut ret, &mem_line, addr as usize)?;
                }
            }
        }

        Ok(ret)
    } else {
        MemResult::Err("")
    }
}

/// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
///
/// Will overwrite memory in modification range.
pub fn insert_code_at(mem: &mut [CPUByte; CPU_MEMSIZE], code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
    if idx <= CPU_MEMSIZE && code.len() + idx <= CPU_MEMSIZE {
        for (offset, &byte) in code.iter().enumerate() {
            mem[idx + offset] = byte;
        }
        Ok(())
    } else if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    }
}

