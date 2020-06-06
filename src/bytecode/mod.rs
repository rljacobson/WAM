/*!

  The VM uses a 32 bit little-endian word size. Instructions are either 32 or 64
  bits (including opcode, see last paragraph). Memory blocks are 64 bit aligned, and
  instructions within memory are 32 bit aligned, with trailing padding as necessary.
  Instructions are decoded bytewise. Memory addresses are 24 bits (3 bytes) and
  address words, not bytes. The sizes of instruction components are as follows:

    Opcode:   8 bits
    Address: 24 bits
    Arity:   24 bits

  Functors do not appear explicitly in the bytecode. They are symbolic representations
  labeling locations in code memory. During compilation and optionally for execution
  (for diagnostic or pedagogic purposes),  functors are resolved to their address via
  a lookup in a symbol table maintained for that purpose.

  One design decision that needed to be made is whether to store the arguments of the
  instruction as data members of enum variants, with one variant per opcode. Rust enums use
  an 8 bit discriminant and have size equal to the largest data payload plus the discriminant.
  (In certain circumstances not relevant here, the compiler can optimize enum values to be
  smaller.) Since the largest instruction is 48 bits excluding opcode, the equivalent enum values
  are 56 bits. Even without considering data alignment, this would waste ~50% of allocated
  memory for instructions on average, which is unacceptably wasteful. Instead, an enum is
  only used for the opcode itself, not the entire instruction, and inhabits a single byte.

*/

mod binary;
mod instruction;
// mod assembly;

pub use binary::{encode_instruction, try_decode_instruction, is_double_word_instruction,
                 EncodedInstruction, Word, TwoWords};
pub use instruction::{Instruction, Operation};
