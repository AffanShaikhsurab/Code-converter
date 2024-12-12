import cobol2python

program = cobol2python.from_cobol("File1", "VAR1 = VAR1 + VAR2\nDISPLAY "File1: VAR1 after addition = " VAR1")

VAR1_value = program.run()['VAR1']
assert VAR1_value == 3000