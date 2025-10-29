use crate::prelude::*;

/// Return new zeroed cpu memory.
pub fn mem_zeros() -> [CPUByte; CPU_MEMSIZE] {
    [0; CPU_MEMSIZE]
}

/// Return new zeroed cpu memory with `code` inserted in range `mem[idx]` - `mem[idx + code.len()`.
pub fn mem_with_code_at(code: &[CPUByte], idx: usize) -> Result<[CPUByte; CPU_MEMSIZE], &'static str> {
    let mut ret = mem_zeros();
    if idx <= CPU_MEMSIZE && code.len() + idx <= CPU_MEMSIZE {
        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }
        Ok(ret)
    } else if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    }
}

/// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
///
/// Will overwrite memory in modification range.
pub fn mem_insert_code_at(mem: &mut [CPUByte; CPU_MEMSIZE], code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
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

