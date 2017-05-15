/**
 * To get rid of the implementation details,
 * the input to the functions should be in types.
 * That can then be controlled internally and
 * externally: external is likely to be messy,
 * so would rather have specific inputs trigger
 * specific types.
 */
type number =
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine;

type sign =
  | Positive
  | Negative;

type operator =
  | Exponent
  | Multiply
  | Divide
  | Add
  | Subtract;

type associativity =
  | Left
  | Right;

let precedence operation =>
  switch operation {
  | Exponent => 4
  | Multiply => 3
  | Divide => 3
  | Add => 2
  | Subtract => 2
  | _ => (-1)
  };

let assoc operation =>
  switch operation {
  | Exponent => Right
  | _ => Left
  };

type input =
  | Number
  | Sign
  | DecimalPoint
  | Operator
  | LeftParen
  | RightParen;


/**
 * Constructing numbers:
 * Numbers in the calculater are _always_
 * floats, with a value defaulting to 0.0.
 *
 * The decimal point is always at the right
 *
 * Numbers come in one-by-one, as strings.
 * Once a non-numeric input comes in, these
 * strings need to be joined, converted to a
 * number proper, and pushed to the outputQueue.
 *
 * Having them represented as individual chars
 * means it becomes simple to execute backspace
 * operations.
 *
 * There are two other operations that can be
 * done that affect individual numbers:
 *     ± plus/minus conversion
 *     . decimal point
 *
 * Decimal point can be used once and only
 * once for any discrete number. Plus/minus
 * can be applied any number of times.
 */
/* let numParse input acc => {
     let hasDecPoint = {contents: false};
     switch input {
     | Number
     | Sign => [input, ...acc]
     | DecimalPoint when not hasDecPoint =>
       hasDecPoint.contents = true;
       [input, ...acc]
     | DecimalPoint => acc
     | _ => List.rev acc
     }
   }; */

/**
 * Implementation of the shunting yard algorithm,
 * used to process the input, taken verbatim rom Wikipedia:
 */
/* While there are tokens to be read: */
let reader token (outputQueue, operatorStack) =>
  /* Read a token: */
  switch token {
  /* If the token is a number, then push it to the output queue: */
  | Number => Queue.push token outputQueue
  /* If the token is a function token, then push it onto the stack:
   * | Function => do some stuff (sin cos etc)
   * If the token is a function argument separator (e.g., a comma):
   * | FunctionSeperator => {
   *    Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue. If no left parentheses are encountered, either the separator was misplaced or parentheses were mismatched.
   * If the token is an operator, o1, then: */
  | Operator =>
    /* while there is an operator token o2, at the top of the operator stack: */
    while (not (Stack.is_empty operatorStack) && Stack.top operatorStack == Operator) {
      let o1 = token;
      let o2 = Stack.top operatorStack;
      /* o1 is left-associative and its precedence is less than or equal to that of o2, or
       * o1 is right associative, and has precedence less than that of o2: */
      if (
        (assoc o1 == Left && precedence o1 <= precedence o2) ||
        (assoc o1 == Right && precedence o1 < precedence o2)
      ) {
        /* pop o2 off the operator stack, onto the output queue: */
        Queue.push (Stack.pop operatorStack) outputQueue
      }
    };
    /* at the end of iteration push o1 onto the operator stack: */
    Stack.push token operatorStack
  /* If the token is a left parenthesis (i.e. "("), then push it onto the stack: */
  | LeftParen => Stack.push token operatorStack
  /* If the token is a right parenthesis (i.e. ")"): */
  | RightParen => {
      /** Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue.
       *  Pop the left parenthesis from the stack, but not onto the output queue.
       *  If the token at the top of the stack is a function token, pop it onto the output queue.
       * If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
    }
  /*
   * When there are no more tokens to read:
   *
   *     - While there are still operator tokens in the stack:
   *
   *         - If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses.
   *         - Pop the operator onto the output queue.
   *
   * Exit.
   */
  };
/*let shuntOperation token (outputQueue, operatorStack) => {
    switch token {
      | Number => Queue.push token outputQueue
      | Operator => {

          processOperator token (outputQueue, operatorStack);
        }
    };
    (outputQueue, operatorStack)
  }; */
/* let processOperator operator (outputQueue, operatorStack) => {
     let break = {contents:false};
     while (not Stack.is_empty operatorStack && not break) {
       let topOp = Stack.top operatorStack;
       let moveToQueue operator => {
         switch (assoc operator) {
         | Left => precedence operator <= precedence topOp
         | Right => precedence operator < precedence topOp
         };
       };
       if (moveToQueue) {
         Queue.push topOp outputQueue;
       } else {
         break.contents = true;
       }
     }
     Stack.push op operatorStack;
   }; */
/* let operatorStack = Stack.create (); */
/* let outputQueue = Queue.create (); */
