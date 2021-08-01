BITS 16]	;tell the assembler that its a 16 bit code
[ORG 0x7C00]	;Origin, tell the assembler that where the code will 
;be in memory after it is been loaded

KERNEL_OFFSET equ 0xF000

%define CODE_SEG     0x0008
%define DATA_SEG     0x0010

    ; enable a20 line http://www.ctyme.com/intr/rb-1336.htm
    mov ax, 0x2401
    int 0x15


    xor ax, ax ; ax = 0
    mov es, ax

    not ax ; ax = 0xFFFF
    mov ds, ax

    ; 0500 and 0510 are chosen since they are guaranteed to be free for use at
    ; any point of time after BIOS initialization.
    mov di, 0x0500
    mov si, 0x0510

    ; [es:di] is 0:0500, [ds:si] is FFFF:0510. the respective physical addresses
    ; for those will be 00500 and 100500.
    mov byte [es:di], 0x00
    mov byte [ds:si], 0xFF

    ; if the A20 line is disabled, [es:di] will contain 0xFF, as the write to
    ; [ds:si] really occured to 00500.
    cmp byte [es:di], 0xFF

    jne _else
    call fail
    jmp end_cond
 _else:
    call suc
end_cond:


Main:
    jmp 0x0000:FlushCS ; Some BIOS' may load us at 0x0000:0x7C00 while other may load us at 0x07C0:0x0000.
    ; Do a far jump to fix this issue, and reload CS to 0x0000.
 
FlushCS:
    call load_kernel

    xor ax, ax
 
    ; Set up segment registers.
    mov ss, ax
    ; Set up stack so that it starts below Main.
    mov sp, Main
 
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    cld




    mov edi, 0x1000    ; Set the destination index to 0x1000.
    mov cr3, edi       ; Set control register 3 to the destination index.
    xor eax, eax       ; Nullify the A-register.
    mov ecx, 4096      ; Set the C-register to 4096.
    rep stosd          ; Clear the memory.
    mov edi, cr3       ; Set the destination index to control register 3.


    mov DWORD [edi], 0x2003      ; Set the uint32_t at the destination index to 0x2003.
    add edi, 0x1000              ; Add 0x1000 to the destination index.
    mov DWORD [edi], 0x3003      ; Set the uint32_t at the destination index to 0x3003.
    add edi, 0x1000              ; Add 0x1000 to the destination index.
    mov DWORD [edi], 0x4003      ; Set the uint32_t at the destination index to 0x4003.
    add edi, 0x1000              ; Add 0x1000 to the destination index.

    mov ebx, 0x00000003          ; Set the B-register to 0x00000003.
    mov ecx, 512                 ; Set the C-register to 512.
 
SetEntry:
    mov DWORD [edi], ebx         ; Set the uint32_t at the destination index to the B-register.
    add ebx, 0x1000              ; Add 0x1000 to the B-register.
    add edi, 8                   ; Add eight to the destination index.
    loop SetEntry

    mov eax, cr4                 ; Set the A-register to control register 4.
    or eax, 1 << 5               ; Set the PAE-bit, which is the 6th bit (bit 5).
    mov cr4, eax                 ; Set control register 4 to the A-register.

    mov ecx, 0xC0000080          ; Set the C-register to 0xC0000080, which is the EFER MSR.
    rdmsr                        ; Read from the model-specific register.
    or eax, 1 << 8               ; Set the LM-bit which is the 9th bit (bit 8).
    wrmsr                        ; Write to the model-specific register.

    mov eax, cr0                 ; Set the A-register to control register 0.
    or eax, 1 << 31 | 1 << 0     ; Set the PG-bit, which is the 31nd bit, and the PM-bit, which is the 0th bit.
    mov cr0, eax                 ; Set control register 0 to the A-register.






    lgdt [GDT.Pointer]                ; Load GDT.Pointer defined below.
 
    jmp CODE_SEG:LongMode             ; Load CS with 64 bit segment and flush the instruction cache
 
 
    ; Global Descriptor Table
GDT:
    .Null:
        dq 0x0000000000000000             ; Null Descriptor - should be present.
    
    .Code:
        dq 0x00209A0000000000             ; 64-bit code descriptor (exec/read).
        dq 0x0000920000000000             ; 64-bit data descriptor (read/write).
    
    ALIGN 4
        dw 0                              ; Padding to make the "address of the GDT" field aligned on a 4-byte boundary
    
    .Pointer:
        dw $ - GDT - 1                    ; 16-bit Size (Limit) of GDT.
        dd GDT                            ; 32-bit Base Address of GDT. (CPU will zero extend to 64-bit)
    

; Functions    
fail:
    mov ah, 0x0e
    mov al, 0x46
    int 0x10
    ret
suc:
    mov ah, 0x0e
    mov al, 0x4F
    int 0x10
    ret


load_kernel:
    mov bx, KERNEL_OFFSET ; Read from disk and store in 0x1000
    mov dh, 2
    mov dl, [BOOT_DRIVE]
    call disk_load
    ret

disk_load:
    pusha
    ; reading from disk requires setting specific values in all registers
    ; so we will overwrite our input parameters from 'dx'. Let's save it
    ; to the stack for later use.
    push dx

    mov ah, 0x02 ; ah <- int 0x13 function. 0x02 = 'read'
    mov al, dh   ; al <- number of sectors to read (0x01 .. 0x80)
    mov cl, 0x02 ; cl <- sector (0x01 .. 0x11)
                 ; 0x01 is our boot sector, 0x02 is the first 'available' sector
    mov ch, 0x00 ; ch <- cylinder (0x0 .. 0x3FF, upper 2 bits in 'cl')
    ; dl <- drive number. Our caller sets it as a parameter and gets it from BIOS
    ; (0 = floppy, 1 = floppy2, 0x80 = hdd, 0x81 = hdd2)
    mov dh, 0x00 ; dh <- head number (0x0 .. 0xF)

    ; [es:bx] <- pointer to buffer where the data will be stored
    ; caller sets it up for us, and it is actually the standard location for int 13h
    int 0x13      ; BIOS interrupt
    jc disk_error ; if error (stored in the carry bit)

    pop dx
    cmp al, dh    ; BIOS also sets 'al' to the # of sectors read. Compare it.
    jne sectors_error
    popa
    ret

disk_error:
    call fail
    jmp disk_loop

sectors_error:
    call fail
    jmp disk_loop

disk_loop:
    jmp $



[BITS 64]      
LongMode:
    cli
    
    mov ax, DATA_SEG
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
 
    ; Blank out the screen to a blue color.
    mov edi, 0xB8000
    mov rcx, 500                      ; Since we are clearing uint64_t over here, we put the count as Count/4.
    mov rax, 0x1F201F201F201F20       ; Set the value to set the screen to: Blue background, white foreground, blank spaces.
    rep stosq                         ; Clear the entire screen. 
 
    ; Display "Hello World!"
    mov edi, 0x00b8000              
 
    mov rax, 0x1F6C1F6C1F651F48    
    mov [edi],rax
 
    mov rax, 0x1F6F1F571F201F6F
    mov [edi + 8], rax
 
    mov rax, 0x1F211F641F6C1F72
    mov [edi + 16], rax

    call KERNEL_OFFSET

inf_loop:
    jmp inf_loop


    BOOT_DRIVE db 0
    TIMES 510 - ($ - $$) db 0	;fill the rest of sector with 0
    DW 0xAA55			; add boot signature at the end of bootloader











































































































; [BITS 16]	;tell the assembler that its a 16 bit code
; [ORG 0x7C00]	;Origin, tell the assembler that where the code will 
; ;be in memory after it is been loaded

; KERNEL_OFFSET equ 0x1000

; %define CODE_SEG    0x0008
; %define DATA_SEG    0x0010

; ; Global Descriptor Table
; GDT:
;     .Null:
;         dq 0x0000000000000000             ; Null Descriptor - should be present.
    
;     .Code:
;         dq 0x00209A0000000000             ; 64-bit code descriptor (exec/read).
;         dq 0x0000920000000000             ; 64-bit data descriptor (read/write).
    
;     ALIGN 4
;         dw 0                              ; Padding to make the "address of the GDT" field aligned on a 4-byte boundary
    
;     .Pointer:
;         dw $ - GDT - 1                    ; 16-bit Size (Limit) of GDT.
;         dd GDT                            ; 32-bit Base Address of GDT. (CPU will zero extend to 64-bit)
    

;     ; save boot drive
;     mov [BOOT_DRIVE], dl

;     ; set stack
;     mov bp, 0x9000
;     mov sp, bp

;     ; enable a20 line http://www.ctyme.com/intr/rb-1336.htm
;     mov ax, 0x2401
;     int 0x15


;     xor ax, ax ; ax = 0
;     mov es, ax

;     not ax ; ax = 0xFFFF
;     mov ds, ax

;     ; 0500 and 0510 are chosen since they are guaranteed to be free for use at
;     ; any point of time after BIOS initialization.
;     mov di, 0x0500
;     mov si, 0x0510

;     ; [es:di] is 0:0500, [ds:si] is FFFF:0510. the respective physical addresses
;     ; for those will be 00500 and 100500.
;     mov byte [es:di], 0x00
;     mov byte [ds:si], 0xFF

;     ; if the A20 line is disabled, [es:di] will contain 0xFF, as the write to
;     ; [ds:si] really occured to 00500.
;     cmp byte [es:di], 0xFF

;     jne _else
;     call fail
;     jmp end_cond
;  _else:
;     call suc
; end_cond:


; Main:
;     jmp 0x0000:FlushCS ; Some BIOS' may load us at 0x0000:0x7C00 while other may load us at 0x07C0:0x0000.
;     ; Do a far jump to fix this issue, and reload CS to 0x0000.
 
; FlushCS:

;     ; call load_kernel

;     xor ax, ax
 
;     ; Set up segment registers.
;     mov ss, ax
;     ; Set up stack so that it starts below Main.
;     mov sp, Main
 
;     mov ds, ax
;     mov es, ax
;     mov fs, ax
;     mov gs, ax
;     cld




;     mov edi, 0x1000    ; Set the destination index to 0x1000.
;     mov cr3, edi       ; Set control register 3 to the destination index.
;     xor eax, eax       ; Nullify the A-register.
;     mov ecx, 4096      ; Set the C-register to 4096.
;     rep stosd          ; Clear the memory.
;     mov edi, cr3       ; Set the destination index to control register 3.


;     mov DWORD [edi], 0x2003      ; Set the uint32_t at the destination index to 0x2003.
;     add edi, 0x1000              ; Add 0x1000 to the destination index.
;     mov DWORD [edi], 0x3003      ; Set the uint32_t at the destination index to 0x3003.
;     add edi, 0x1000              ; Add 0x1000 to the destination index.
;     mov DWORD [edi], 0x4003      ; Set the uint32_t at the destination index to 0x4003.
;     add edi, 0x1000              ; Add 0x1000 to the destination index.

;     mov ebx, 0x00000003          ; Set the B-register to 0x00000003.
;     mov ecx, 512                 ; Set the C-register to 512.
 
; SetEntry:
;     mov DWORD [edi], ebx         ; Set the uint32_t at the destination index to the B-register.
;     add ebx, 0x1000              ; Add 0x1000 to the B-register.
;     add edi, 8                   ; Add eight to the destination index.
;     loop SetEntry

;     mov eax, cr4                 ; Set the A-register to control register 4.
;     or eax, 1 << 5               ; Set the PAE-bit, which is the 6th bit (bit 5).
;     mov cr4, eax                 ; Set control register 4 to the A-register.

;     mov ecx, 0xC0000080          ; Set the C-register to 0xC0000080, which is the EFER MSR.
;     rdmsr                        ; Read from the model-specific register.
;     or eax, 1 << 8               ; Set the LM-bit which is the 9th bit (bit 8).
;     wrmsr                        ; Write to the model-specific register.

;     mov eax, cr0                 ; Set the A-register to control register 0.
;     or eax, 1 << 31 | 1 << 0     ; Set the PG-bit, which is the 31nd bit, and the PM-bit, which is the 0th bit.
;     mov cr0, eax                 ; Set control register 0 to the A-register.
    
;     lgdt [GDT.Pointer]                ; Load GDT.Pointer defined below.
 
;     jmp CODE_SEG:LongMode             ; Load CS with 64 bit segment and flush the instruction cache
 

; ; Functions 16 bits
; fail:
;     mov ah, 0x0e
;     mov al, 0x46
;     int 0x10
;     ret
; suc:
;     mov ah, 0x0e
;     mov al, 0x4F
;     int 0x10
;     ret

; load_kernel:
;     ; pusha
;     ; push dx

;     ; xor ax, ax ; ax = 0
;     ; mov es, ax

;     ; mov bx, KERNEL_OFFSET ; read to KERNEL_OFFSET location
;     ; mov dl, [BOOT_DRIVE] ; read from drive [BOOT_DRIVE]
;     ; mov ah, 0x02 ; read mode
;     ; mov al, 2   ; read 2 sectors
;     ; mov cl, 0x02 ; start from sector 2
;     ;              ; (as sector 1 is our boot sector)
;     ; mov ch, 0x00 ; cylinder 0
;     ; mov dh, 0x00 ; head 0

;     ; ; dl = drive number is set as input to disk_load
;     ; ; es:bx = buffer pointer is set as input as well

;     ; int 0x13      ; BIOS interrupt
;     ; jc disk_error ; check carry bit for error

;     ; pop dx     ; get back original number of sectors to read
;     ; cmp al, 2 ; BIOS sets 'al' to the # of sectors actually read
;     ;            ; compare it to 'dh' and error out if they are !=
;     ; jne sectors_error
;     ; popa
;     ; ret

;     mov bx, KERNEL_OFFSET ; Read from disk and store in 0x1000
;     mov dh, 2
;     mov dl, [BOOT_DRIVE]
;     call disk_load
;     ret

; disk_load:
;     pusha
;     ; reading from disk requires setting specific values in all registers
;     ; so we will overwrite our input parameters from 'dx'. Let's save it
;     ; to the stack for later use.
;     push dx

;     mov ah, 0x02 ; ah <- int 0x13 function. 0x02 = 'read'
;     mov al, dh   ; al <- number of sectors to read (0x01 .. 0x80)
;     mov cl, 0x02 ; cl <- sector (0x01 .. 0x11)
;                  ; 0x01 is our boot sector, 0x02 is the first 'available' sector
;     mov ch, 0x00 ; ch <- cylinder (0x0 .. 0x3FF, upper 2 bits in 'cl')
;     ; dl <- drive number. Our caller sets it as a parameter and gets it from BIOS
;     ; (0 = floppy, 1 = floppy2, 0x80 = hdd, 0x81 = hdd2)
;     mov dh, 0x00 ; dh <- head number (0x0 .. 0xF)

;     ; [es:bx] <- pointer to buffer where the data will be stored
;     ; caller sets it up for us, and it is actually the standard location for int 13h
;     int 0x13      ; BIOS interrupt
;     jc disk_error ; if error (stored in the carry bit)

;     pop dx
;     cmp al, dh    ; BIOS also sets 'al' to the # of sectors read. Compare it.
;     jne sectors_error
;     popa
;     ret

; disk_error:
;     call fail
;     jmp disk_loop

; sectors_error:
;     call fail
;     jmp disk_loop

; disk_loop:
;     jmp $

; ; ===============================================



 
; [BITS 64]

; LongMode:
;     cli
    
;     mov ax, DATA_SEG
;     mov ds, ax
;     mov es, ax
;     mov fs, ax
;     mov gs, ax
;     mov ss, ax


;     ; Blank out the screen to a blue color.
;     mov edi, 0xB8000
;     mov rcx, 500                      ; Since we are clearing uint64_t over here, we put the count as Count/4.
;     mov rax, 0x1F201F201F201F20       ; Set the value to set the screen to: Blue background, white foreground, blank spaces.
;     rep stosq                         ; Clear the entire screen. 
 
;     ; Display "Hello World!"
;     mov edi, 0x00b8000              
 
;     mov rax, 0x1F6C1F6C1F651F48    
;     mov [edi],rax
 
;     mov rax, 0x1F6F1F571F201F6F
;     mov [edi + 8], rax
 
;     mov rax, 0x1F211F641F6C1F72
;     mov [edi + 16], rax

; inf_loop:
;     jmp inf_loop
 
;     call KERNEL_OFFSET



















 
    
;     BOOT_DRIVE db 0

;     TIMES 510 - ($ - $$) db 0	;fill the rest of sector with 0
;     DW 0xAA55			; add boot signature at the end of bootloader