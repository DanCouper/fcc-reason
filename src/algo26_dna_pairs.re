/**
 * Base pairs are a pair of AT and CG. Match the missing element
 * to the provided character. Return the provided character as the
 * first element in each list.
 * For example, for the input GCG, return `[['G', 'C'], ['C','G'],['G', 'C']]
 */
/* let split_on_char sep s => {
     let r = ref [];
     let j = ref (String.length s);
     for i in (String.length s - 1) downto 0 {
       if (s.[i] == sep) {
         r := [String.sub s (i + 1) (!j - i - 1), ...!r];
         j := i
       }
     };
     [String.sub s 0 !j, ...!r]
   }; */
exception NotDNA string;

let compute dnaStr => {
  let rec loop i acc =>
    if (i < 0) {
      acc
    } else {
      switch dnaStr.[i] {
      | 'A' => loop (i - 1) [['A', 'T'], ...acc]
      | 'T' => loop (i - 1) [['T', 'A'], ...acc]
      | 'C' => loop (i - 1) [['C', 'G'], ...acc]
      | 'G' => loop (i - 1) [['G', 'C'], ...acc]
      | _ => raise (NotDNA "invalid strand, non-dna code detected")
      }
    };
  loop (String.length dnaStr - 1) []
};
