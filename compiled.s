		.data
int1:	.word	1
int2:	.word	2
int3:	.word	3

		.text
main:
	# (CONST 2)
	addi $sp, $sp, -4
	lw $t0, int2
	sw $t0, 0($sp)

	# (CONST 3)
	addi $sp, $sp, -4
	lw $t0, int3
	sw $t0, 0($sp)

	# (ADD)
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	add $t2, $t1, $t0
	sw $t2, 0($sp)

	# (CONST 1)
	addi $sp, $sp, -4
	lw $t0, int1
	sw $t0, 0($sp)

	# (SUB)
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sub $t2, $t1, $t0
	sw $t2, 0($sp)

	li	$v0, 1
	lw	$a0, 0($sp)
	syscall
	li	$v0, 10
	syscall
