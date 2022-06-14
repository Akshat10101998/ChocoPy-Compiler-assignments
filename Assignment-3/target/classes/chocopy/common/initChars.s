  la a0, $str$prototype                    # prototype to str
  lw t0, 0(a0)                             # Load type tag
  lw t1, 4(a0)                             # Load size in words
  lw t2, 8(a0)                             # Load pointer to dispatch table
  li t3, 1                                 
  la a0, allChars                          # Load addr to data table
  li t4, 256                               # load max char decimal value
  mv t5, zero                              
initchars_init:                            # init single char str
  sw t0, 0(a0)                             # Set type tag
  sw t1, 4(a0)                             # Set size in words
  sw t2, 8(a0)                             # Set pointer to dispatch table
  sw t3, 12(a0)                            # Set __len__ attr
  sw t5, 16(a0)                            # Set char value
  addi a0, a0, 20                          # Move pointer to next free space
  addi t5, t5, 1                           # Increment counter
  bne t4, t5, initchars_init               # Continue init with condition
  jr ra                                    # Return 
 .data
     