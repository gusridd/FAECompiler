		.data
int0:	.word	0

		.text
	
control_hack:
	jr $t1
	
copyEnvReturn:
	# lw $t0, 0($fp)
	jr $ra
	
	
copyEnv: 			# after this call a zero must appear at 0($sp)
	beq $t8, $0, copyEnvReturn 	# if env.size == 0 return
	lw $t0, 0($sp)
	addi $t3, $t0, 3 	# t3 = t0 + 3 (for the old_fp and old_sp)
	li $t1, 4
	mult $t3,$t1
	mflo $t3 		# t3 = t3 * 4 (alignment)
	add $t4, $t3, $fp
	# sw ($t4), 0($sp) 	# sp[0] = fp[t3] ($t4 is positioned at env[arg])
	sub $t5, $t8, $t0 	# t5 = env-size - arg
	mult $t5,$t1
	mflo $t5 		# t5 = t5 * 4 (alignment)
	sub $t5, $sp, $t5
	lw $t4, ($t4)
	sw $t4, ($t5) 	# the actual copy
	# addi $sp, $sp, -4 	# (reposition the stack pointer)
	beq $t0, $0, copyEnvReturn	# if(t0 == 0) return
	addi $t3, $t0, -1 	# t3 = t0 - 1
	sw $t3, 0($sp) 		# sp[0] = t3
	j copyEnv
	
fun1: # label for fun1
	addi $t8, $t8, 1 	# env-size + 1
	sw $ra, 0($fp)
	# (ACCESS 1)
	addi $sp, $sp, -4
	li $t1, 2
	addi $t0, $t1, 1
	li $t1, 4
	mult $t0, $t1
	mflo $t0 		# t3 = t3 * 4 (alignment)
	add $t0, $fp, $t0
	lw $t1, ($t0)
	sw $t1, 0($sp)
	
	# (RETURN)
	lw $t0, 0($sp) 		# return value into $t0
	lw $t7, 0($fp) 		# restore old return address to t7
	lw $t6, 4($fp) 		# restore old frame pointer to t6
	lw $t5, 8($fp) 		# restore old stack pointer to t5
	move $sp, $t5
	move $fp, $t6
	move $ra, $t7
	sw $t0, 0($sp) 		# place return var into stack
	addi $t8, $t8, -1 	# env-size - 1
	jr	$ra

main:
	add $fp, $sp, 4
	# (CLOSURE_CONST fun1)
	addi $sp, $sp, -4
	la $t0, fun1
	sw $t0, 0($sp)

	# (CONST 0)
	addi $sp, $sp, -4
	lw $t0, int0
	sw $t0, 0($sp)

	# (APPLY)
	lw $t0, 0($sp) 		# argument into $t0
	addi $t3,$t8,2 		# t3 = env-size + 2
	li $t1, 4
	mult $t3, $t1
	mflo $t3 		# t3 = t3 * 4 (alignment)
	sub $t3, $sp, $t3
	sw $t0, ($t3) 		# sp[t3] = t0
	addi $sp, $sp, -4
	lw $t8, 0($sp) 		# argument for copyEnv
	jal copyEnv
	addi $t3, $t8, 2
	li $t1, 4
	mult $t3, $t1
	mflo $t3 		# t3 = t3 * 4 (alignment)
	sub $t3, $sp, $t3
	addi $sp, $sp, 8
	sw $sp, ($t3) 		# save old stack pointer
	addi $t3, $t3, -4
	sw $fp, ($t3) 		# save old frame pointer
	addi $t3, $t3, -4
	sw $ra, ($t3) 		# save return address
	lw $t1, 0($sp) 		# function location into $t1
	move $sp, $t3 		# move the stack to the new stack position
	move $fp, $sp 		# refresh frame pointer (below stack)
	addiu $t3, $t3, -4 	# position t3 at new stack position
	jal $t1 		# call function

	li	$v0, 1
	lw	$a0, 0($sp)
	syscall
	li	$v0, 10
	syscall
