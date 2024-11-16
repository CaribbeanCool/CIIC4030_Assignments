import ply.yacc as yacc
import ply.lex as lex

# List of all token names
tokens = [
    'IDENTIFIER', 'NUMBER',
    'IF', 'ELSE', 'WHILE', 'LET', 'LOOP', 'FN', 'MUT', 'PUB', 'REF', 'IN',
    'STRUCT', 'TYPE', 'RETURN', 'TRUE', 'FALSE', 'WHERE', 'WRITE',
    'LPAREN', 'RPAREN', 'LCURLY', 'RCURLY', 'LSQR', 'RSQR', 'SEMICOLON', 'COMMA',
    'EQUALS', 'NEQ', 'LEQ', 'GEQ', 'LT', 'GT', 'PLUS', 'MINUS', 'STAR', 'SLASH', 'MOD', 'INT', 'FLOAT', 'CHAR', 'BOOLEAN',
]

# Regular expression rules
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LCURLY = r'\{'
t_RCURLY = r'\}'
t_LSQR = r'\['
t_RSQR = r'\]'
t_SEMICOLON = r';'
t_COMMA = r','
t_EQUALS = r'='
t_NEQ = r'!='
t_LEQ = r'<='
t_GEQ = r'>='
t_LT = r'<'
t_GT = r'>'
t_PLUS = r'\+'
t_MINUS = r'-'
t_STAR = r'\*'
t_SLASH = r'/'
t_MOD = r'%'

# Type tokens
t_INT = r'int'
t_FLOAT = r'float'
t_CHAR = r'char'
t_BOOLEAN = r'boolean'

# Reserved keywords
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'let': 'LET',
    'loop': 'LOOP',
    'fn': 'FN',
    'mut': 'MUT',
    'pub': 'PUB',
    'ref': 'REF',
    'in': 'IN',
    'struct': 'STRUCT',
    'type': 'TYPE',
    'return': 'RETURN',
    'true': 'TRUE',
    'false': 'FALSE',
    'where': 'WHERE',
    'write': 'WRITE',
}

# A function to match identifiers (ID) and keywords


def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Check for reserved keywords
    return t

# A function to match numbers


def t_NUMBER(t):
    r'\d+'  # same thing as [0-9]+
    t.value = int(t.value)
    return t

# A function to track line numbers


def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A rule to ignore comments (//) and everything until the end of line


def t_COMMENT(t):
    r'//.*'
    pass


# A rule to ignore whitespace (space, tab, carriage return)
t_ignore = ' \t\r'

# Error handling rule


def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)


# Constructing the lexer
lexer = lex.lex()

# Dictionary to store variables
variables = {}

# Dictionary for storing function definitions
functions = {}

# ==================================================================
#                        PARSING RULES
# ==================================================================

# Define the precedence of operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'STAR', 'SLASH', 'MOD'),
    ('nonassoc', 'LEQ', 'GEQ', 'LT', 'GT'),
    ('nonassoc', 'EQUALS', 'NEQ'),
)

# Parser rules


def p_program(p):
    '''program : program function
               | program struct
               | function
               | struct'''
    # if len(p) == 2:
    #     if p[1] is not None:  # Include valid items
    #         p[0] = ('program', [p[1]])
    #     else:
    #         p[0] = ('program', [])
    # else:
    #     if p[2] is not None:
    #         p[0] = ('program', p[1][1] + [p[2]])
    #     else:
    #         p[0] = p[1]
    if len(p) == 2:
        p[0] = ('program', [p[1]])
    else:
        p[0] = ('program', p[1][1] + [p[2]])


def p_function(p):
    '''function : FN IDENTIFIER LPAREN list_parameters RPAREN return_type LCURLY statements RCURLY'''
    # functions[p[2]] = {'params': p[4], 'return_type': p[6], 'statements': p[8]}
    # using `return_type` instead of `type` to avoid conflict with the `type` token
    p[0] = ('function', p[2], p[4], p[6], p[8])


def p_return_type(p):
    '''return_type : type
                   | empty'''
    p[0] = p[1]


def p_statements_single(p):
    '''statements : statement'''
    p[0] = [p[1]]


def p_statements_multiple(p):
    '''statements : statements statement'''
    if p[2] is not None:
        # Only include valid statements
        p[0] = p[1] + [p[2]]
    else:
        p[0] = p[1]


def p_statement_assignment(p):
    '''statement : assignment_statement'''
    p[0] = p[1]


def p_statement_if(p):
    '''statement : if_statement'''
    p[0] = p[1]


def p_statement_while(p):
    '''statement : while_statement'''
    p[0] = p[1]


def p_statement_action(p):
    '''statement : action_statement'''
    p[0] = p[1]


def p_statement_let(p):
    '''statement : let_expression'''
    p[0] = p[1]


def p_assignment_statement(p):
    '''assignment_statement : IDENTIFIER EQUALS expression SEMICOLON'''
    variables[p[1]] = p[3]
    p[0] = ('assign', p[1], p[3])


def p_if_statement(p):
    '''if_statement : IF expression LCURLY statements RCURLY else_clause'''
    # if p[2]:
    #     p[0] = ('if', p[2], p[4])
    # elif p[6] is not None:
    #     p[0] = p[6]
    p[0] = ('if', p[2], p[4], p[6])


def p_else_clause(p):
    '''else_clause : ELSE LCURLY statements RCURLY
                   | ELSE IF expression LCURLY statements RCURLY else_clause
                   | empty'''
    if len(p) == 5:
        p[0] = ('else', p[3])
    elif len(p) == 8:
        p[0] = ('else_if', p[3], p[5], p[7])
    else:
        p[0] = None


def p_while_statement(p):
    '''while_statement : WHILE LPAREN expression RPAREN LCURLY statements RCURLY'''
    p[0] = ('while', p[3], p[6])


def p_action_statement_return(p):
    '''action_statement : RETURN expression SEMICOLON'''
    p[0] = ('return', p[2])


def p_action_statement_write(p):
    '''action_statement : WRITE expression SEMICOLON'''
    p[0] = ('write', p[2])


def p_action_statement_where(p):
    '''action_statement : WHERE expression SEMICOLON'''
    p[0] = ('where', p[2])


def p_action_statement_function_call(p):
    '''action_statement : function_call'''
    p[0] = p[1]


# List to collect errors during parsing
errors = []

# ==================================================================
#                        EXPRESSION HANDLING
# ==================================================================


def p_let_expression(p):
    '''let_expression : LET IDENTIFIER EQUALS expression SEMICOLON
                      | LET MUT IDENTIFIER EQUALS expression SEMICOLON
                      | LET REF IDENTIFIER EQUALS expression SEMICOLON'''
    if len(p) == 6:
        variables[p[2]] = p[4]
    elif p[2] == 'mut':
        variables[p[3]] = p[5]
    else:
        variables[p[3]] = p[5]
    p[0] = ('let', p[2], p[4] if len(p) == 6 else p[5])


def p_expression_identifier(p):
    '''expression : IDENTIFIER'''
    try:
        if p[1] not in variables:
            raise ExpressionError(f"Undefined variable '{p[1]}'", p.lineno(1))
        p[0] = variables[p[1]]
    except ExpressionError as e:
        errors.append(f"Error at line {e.line}: {e.message}")
        p[0] = None
    except Exception as e:
        errors.append(f"Unexpected error at line {p.lineno(1)}: {str(e)}")
        p[0] = None


def p_expression_operation(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression STAR expression
                  | expression SLASH expression
                  | expression MOD expression
                  | expression EQUALS expression
                  | expression NEQ expression
                  | expression LEQ expression
                  | expression GEQ expression
                  | expression LT expression
                  | expression GT expression'''
    try:
        # Type checking
        if not isinstance(p[1], (int, float)) or not isinstance(p[3], (int, float)):
            raise ExpressionError(
                f"Invalid operands for {p[2]}: {type(p[1]).__name__} and {
                    type(p[3]).__name__}",
                p.lineno(2)
            )

        # Operation-specific checks
        if p[2] == '/':
            if p[3] == 0:
                raise ExpressionError("Division by zero", p.lineno(2))
            p[0] = p[1] / p[3]
        elif p[2] == '%':
            if p[3] == 0:
                raise ExpressionError("Modulo by zero", p.lineno(2))
            p[0] = p[1] % p[3]
        elif p[2] == '+':
            p[0] = p[1] + p[3]
        elif p[2] == '-':
            p[0] = p[1] - p[3]
        elif p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '==':
            p[0] = p[1] == p[3]
        elif p[2] == '!=':
            p[0] = p[1] != p[3]
        elif p[2] == '<=':
            p[0] = p[1] <= p[3]
        elif p[2] == '>=':
            p[0] = p[1] >= p[3]
        elif p[2] == '<':
            p[0] = p[1] < p[3]
        elif p[2] == '>':
            p[0] = p[1] > p[3]
        else:
            raise ExpressionError(f"Invalid operator: {p[2]}", p.lineno(2))

    except ExpressionError as e:
        errors.append(f"Error at line {e.line}: {e.message}")
        p[0] = None
    except Exception as e:
        errors.append(f"Unexpected error at line {p.lineno(2)}: {str(e)}")
        p[0] = None


def p_expression_paren(p):
    '''expression : LPAREN expression RPAREN'''
    try:
        if p[2] is None:
            raise ExpressionError(
                "Invalid expression inside parentheses", p.lineno(1))
        p[0] = p[2]
    except ExpressionError as e:
        errors.append(f"Error at line {e.line}: {e.message}")
        p[0] = None
    except Exception as e:
        errors.append(f"Unexpected error at line {p.lineno(1)}: {str(e)}")
        p[0] = None


def p_expression_number(p):
    '''expression : NUMBER'''
    try:
        if not isinstance(p[1], (int, float)):
            raise ExpressionError(f"Invalid number format: {
                                  p[1]}", p.lineno(1))
        # or p[0] = ('number', p[1])
        p[0] = p[1]
    except ExpressionError as e:
        errors.append(f"Error at line {e.line}: {e.message}")
        p[0] = None
    except Exception as e:
        errors.append(f"Unexpected error at line {p.lineno(1)}: {str(e)}")
        p[0] = None


def p_expression_boolean(p):
    '''expression : TRUE
                  | FALSE'''
    try:
        p[0] = True if p[1] == 'true' else False
    except Exception as e:
        errors.append(f"Unexpected error at line {p.lineno(1)}: {str(e)}")
        p[0] = None


def p_expression_function_call(p):
    '''expression : function_call'''
    try:
        if p[1] is None:
            raise ExpressionError("Invalid function call", p.lineno(1))
        p[0] = p[1]
    except ExpressionError as e:
        errors.append(f"Error at line {e.line}: {e.message}")
        p[0] = None
    except Exception as e:
        errors.append(f"Unexpected error at line {p.lineno(1)}: {str(e)}")
        p[0] = None


def p_struct(p):
    '''struct : STRUCT IDENTIFIER LCURLY struct_statements RCURLY'''
    p[0] = ('struct', p[2], p[4])


def p_struct_statements(p):
    '''struct_statements : let_expression
                         | struct_statements let_expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_list_parameters_empty(p):
    '''list_parameters : '''
    p[0] = []


def p_list_parameters_single(p):
    '''list_parameters : IDENTIFIER type'''
    p[0] = [(p[1], p[2])]


def p_list_parameters_multiple(p):
    '''list_parameters : IDENTIFIER type COMMA list_parameters'''
    p[0] = [(p[1], p[2])] + p[4]


def p_list_arguments_empty(p):
    '''list_arguments : '''
    p[0] = []


def p_list_arguments_single(p):
    '''list_arguments : expression'''
    p[0] = [p[1]]


def p_list_arguments_multiple(p):
    '''list_arguments : list_arguments COMMA expression'''
    p[0] = p[1] + [p[3]]


# ==================================================================
#                        FUNCTION HANDLING
# ==================================================================

def p_function_call(p):
    '''function_call : IDENTIFIER LPAREN list_arguments RPAREN'''
    if p[1] in functions:
        func = functions[p[1]]
        if len(func['params']) != len(p[3]):
            raise ValueError(f"Function '{p[1]}' expects {len(
                func['params'])} arguments, but {len(p[3])} were provided.")
        # Execute function here or return the call for later processing
        # Represents a call to function `p[1]` with arguments `p[3]`
        p[0] = ('call', p[1], p[3])
    else:
        raise NameError(f"Function '{p[1]}' is not defined.")


def p_type(p):
    '''type : INT
            | FLOAT
            | CHAR
            | BOOLEAN
            | IDENTIFIER'''
    p[0] = p[1]


def p_empty(p):
    '''empty :'''
    p[0] = None
# ==================================================================
#                        ERROR HANDLING
# ==================================================================


class ExpressionError(Exception):
    def __init__(self, message: str, line):
        self.message = message
        self.line = line
        super().__init__(self.message)


def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}', line {p.lineno}")

        # General hints based on the token and context
        if p.type in {'RPAREN', 'RCURLY', 'RSQUARE'}:
            print("Hint: Check for a missing operand or mismatched parentheses/braces.")
        elif p.type in {'PLUS', 'MINUS', 'MULT', 'DIV'}:
            print("Hint: Check for missing operands around the operator.")
        elif p.type == 'IDENTIFIER':
            print("Hint: Verify variable or function declarations.")
        else:
            print("Hint: Check syntax around this token.")
    else:
        print("Syntax error at EOF")
        print("Hint: Check for incomplete code or unclosed blocks.")


def parse(data):
    """
    Parse the provided data and return the result.
    If there are errors during parsing, they are printed and None is returned.
    """
    global errors
    errors.clear()

    try:
        result = parser.parse(data, lexer=lexer)

        if errors:
            print("\nErrors encountered during parsing:")
            for error in errors:
                print(error)
            return None

        return result

    except Exception as e:
        print(f"Fatal parsing error: {str(e)}")
        return None


# Build the parser
parser = yacc.yacc()

if __name__ == "__main__":
    with open('./Program_Test.txt', 'r') as tester:
        input_data = tester.read()
        lexer.input(input_data)
        while True:
            token = lexer.token()
            if not token:
                break
        result = parse(input_data)
