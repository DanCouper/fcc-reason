/**
 * TODO
 *
 * - [ ] Set up basic structure using types
 * - [ ] write input parser
 * - [ ] deal with numbers
 * - [ ] write shunting yard
 * - [ ] implement functions
 * - [ ] implement memory
 * - [ ] implement backspace
 * - [ ] implement history
 */

/**
 * The most basic pocket calculator has only
 * standard operations. Operations subsequent to the
 * initial operation act like an equals button, and the
 * operator precedence is irrelevant - the calculations
 * just happen one after another:
 *
 *    1 (displays 1)
 *    + (displays 1)
 *    2 (displays 2)
 *    * (displays 3)
 *    2 (displays 2)
 *    = (displays 6)
 *
 * This calculator accepts arbitrary math expressions,
 * with each expression executed via the equals button.
 *
 *   1 + 2 * 2 = (displays 5)
 *
 * This means the input must be parsed correctly,
 * so a shunting yard algorithm can be used. That in
 * turn means it is important to correctly tokenise the
 * input. A further issue is that the input comes in
 * character-by-character, so digits need to be turned
 * into numbers as they come in.
 */

/**
 * The base types are all explicitly defined. I _could_
 * simply use chars or floats or whatever, but as I wholly
 * control what should be used in the calculator, it makes
 * more sense to extend that control to the type design.
 */
type calculatorDigit =
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | DecimalSeperator;

type calculatorOperator =
  | Exp
  | Mul
  | Div
  | Add
  | Sub;

type calculatorAction =
  | Clear
  | Execute;

type calculatorInput =
  | Digit calculatorDigit
  | Number
  | Action calculatorAction
  /* | Function */
  /* | FunctionSep */
  | Operator calculatorOperator
  | LeftParen
  | RightParen;

let mapInputTypeToString inputType =>
  switch inputType {
  | LeftParen => "("
  | RightParen => ")"
  | Digit d =>
    switch d {
    | Zero => "0"
    | One => "1"
    | Two => "2"
    | Three => "3"
    | Four => "4"
    | Five => "5"
    | Six => "6"
    | Seven => "7"
    | Eight => "8"
    | Nine => "9"
    | DecimalSeperator => "."
    }
  | Operator o =>
    switch o {
    | Exp => "^"
    | Mul => "\195\151"
    | Div => "\195\183"
    | Add => "+"
    | Sub => "-"
    }
  };


/**
 * The state can be used as the base for the React UI's
 * core wrapper component. Needs to get
 * passed along through each core function.
 */
type calculatorState = {
  /**
   * The tokenisedInput is modelled as a list of tuples
   * of the form `(Type, stringRepresentation)`.
   * This means that:
   *
   *    - on execution, the list can be easily parsed
   *      by the shunting yard
   *    - on display, the strings can simply be joined
   *      to give a visual representation of the
   *      calculation.
   */
  tokenisedInputs: list (calculatorInput, string),
  /**
   * At some point, the input has to be displayed,
   * and that display can be a string for simplicity's sake:
   */
  display: string,
  /**
   * While a number is being entered, the
   * input should _not_ be immediately added
   * to tokenisedInput - it needs to be converted
   * to a type Number.
   * There can be multiple digits, so while
   * input is of the type Digit, this list gets
   * populated. Once non-digit input arrives,
   * the list gets concatenated, and the resulting
   * number added to the tokenised input:
   */
  numberBuilder: list string,
  /**
   * A number can have one decimal seperator;
   * if this switches to true, and isNumericInput
   * is also true, no more decimal seperators can
   * be added.
   */
  hasDecimalSeperator: bool
};


/**
 * Triggered on an input of type Execute.
 * Takes current state of tokenisedInput and
 * runs the shunting yard.
 */
/* type calculate = tokenisedInputs => tokenisedInputs; */

/**
 * Given an input, update the state accordingly.
 * Needs to use the flags within the state to
 * deal with numbers etc.
 */
type updateInput = (calculatorInput, calculatorState) => calculatorState;

let updateInput (calculatorInput, calculatorState) => {
  let {tokenisedInputs, display, numberBuilder, hasDecimalSeperator} = calculatorState;
  switch calculatorInput {
  | Digit d =>
    let digit = mapInputTypeToString calculatorInput;
    switch d {
    | DecimalSeperator when hasDecimalSeperator => calculatorState
    | DecimalSeperator => {
        ...calculatorState,
        display: display ^ digit,
        numberBuilder: [digit, ...numberBuilder],
        hasDecimalSeperator: true
      }
    | _ => {...calculatorState, display: display ^ digit, numberBuilder: [digit, ...numberBuilder]}
    }
  | _ when numberBuilder != [] =>
    let num = List.fold_right (fun prev curr => prev ^ curr) numberBuilder "";
    {
      display: String.concat " " [display, num, mapInputTypeToString calculatorInput],
      tokenisedInputs: [
        (calculatorInput, mapInputTypeToString calculatorInput),
        (Number, num),
        ...tokenisedInputs
      ],
      numberBuilder: [],
      hasDecimalSeperator: false
    }
  | _ => {
      ...calculatorState,
      display: String.concat " " [display, mapInputTypeToString calculatorInput],
      tokenisedInputs: [
        (calculatorInput, mapInputTypeToString calculatorInput),
        ...tokenisedInputs
      ]
    }
  }
};


/**
 * On an update to the tokenisedInput,
 * just rebuild the display
 */
type updateDisplayFromInput = calculatorState => calculatorState;
/**
 * Input to the Calculator builds up a list of
 * the form `(Token, string)` that represents
 * current input. The input comes from the Key
 * components. The strings are extracted
 * and passed to the Display component.
 *
 * At the expense of needing to manually execute
 * calculations, this implementation allows
 * complex mathematical expressions: On execute,
 * the token list is handed to a shunting yard
 * algorithm that processes the expression.
 *
 * NOTE because the characters come in one-by-one,
 * this raises an issue that the Calculator must
 * know what type of input is coming in to accurately
 * represent numbers. It needs to be able to
 * build a `Number` from streams of `Digit` types,
 * and it is helpful if the input is tagged as
 * early as possible in the process to aid in
 * pattern matching.
 *
 * TODO Going forward, the history can be passed
 * to a discrete History component
 */
/* module Calculator = {
     include ReactRe.Component.Stateful;
     type state = {
       inputVals: list (token, string),
       numBuilder: list string,
       isNumericInput: bool,
       isNegativeNum: bool,
       isDecimalNum: bool,
       outputStr: string
     };
     type props = unit;
     let name = "display";
     let getInitialState _ => {
       inputVals: [],
       numBuilder: [],
       isNumericInput: false,
       isNegativeNum: false,
       isDecimalNum: false,
       outputStr: "0.0"
     };
     let handleInput {state} event => {};
     let handleExecute {state} => {};
     let handleClear {state} => {};
   };

   module Key = {
     include ReactRe.Component;
   };

   module Display = {
     include ReactRe.Component;
   }; */
