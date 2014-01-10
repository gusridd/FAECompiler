		.data
int5:	.word	5
int1:	.word	1
int2:	.word	2

		.text

main:					# indicates start of code (first instruction to execute)

	addi $sp, $sp, -4	# (CONST 5)
	lw	$t0, int5
	sw	$t0, 0($sp) 		

	addi $sp, $sp, -4	# (CONST 1)
	lw	$t0, int1
	sw	$t0, 0($sp) 		

	lw $t0, 0($sp)		# (ADD)
	addi $sp, $sp, 4	
	lw $t1, 0($sp)
	add	$t2, $t0, $t1
	sw	$t2, 0($sp)

	li	$v0, 1 		# load appropriate system call code into register $v0;
 						# code for printing integer is 1
	lw	$a0, 0($sp)		# move integer to be printed into $a0:  $a0 = $t2
	# sw $a0, 0($sp)
	syscall				# call operating system to perform operation
	li	$v0, 10			# system call code for exit = 10
	syscall				# call operating sys

