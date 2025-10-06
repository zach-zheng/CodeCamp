# Build an Arithmetic Formatter Project

def arithmetic_arranger(problems, show_answers=False):
    if len(problems) > 5:
        return 'Error: Too many problems.'

    first_operands = []
    second_operands = []
    operators = []
    answers = []

    for problem in problems:
        parts = problem.split()
        if len(parts) != 3:
            return 'Error: Invalid problem format.'

        first_operand, operator, second_operand = parts

        if operator not in ['+', '-']:
            return "Error: Operator must be '+' or '-'."

        if not first_operand.isdigit() or not second_operand.isdigit():
            return 'Error: Numbers must only contain digits.'

        if len(first_operand) > 4 or len(second_operand) > 4:
            return 'Error: Numbers cannot be more than four digits.'

        first_operands.append(first_operand)
        second_operands.append(second_operand)
        operators.append(operator)

        if operator == '+':
            answer = str(int(first_operand) + int(second_operand))
        else:
            answer = str(int(first_operand) - int(second_operand))
        
        answers.append(answer)

    arranged_problems = []
    top_row = []
    bottom_row = []
    dash_row = []
    answer_row = []

    for i in range(len(problems)):
        width = max(len(first_operands[i]), len(second_operands[i])) + 2
        top_row.append(first_operands[i].rjust(width))
        bottom_row.append(operators[i] + ' ' + second_operands[i].rjust(width - 2))
        dash_row.append('-' * width)
        answer_row.append(answers[i].rjust(width))

    arranged_problems.append('    '.join(top_row))
    arranged_problems.append('    '.join(bottom_row))
    arranged_problems.append('    '.join(dash_row))

    if show_answers:
        arranged_problems.append('    '.join(answer_row))

    return '\n'.join(arranged_problems)

# Test the function
print(arithmetic_arranger(["32 + 698", "3801 - 2", "45 + 43", "123 + 49"]))
print(arithmetic_arranger(["32 + 8", "1 - 3801", "9999 + 9999", "523 - 49"], True))