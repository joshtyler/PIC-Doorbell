; File doorbell_main.asm
; CODE FOR DOORBELL RECEIVER
; LDPS III Lab Project: Joshua Tyler, Le Hoang, Vilme Wilke and Umair Hassan
; Purpose:
    ; Plays 4 different melodies when 4 different buttons are pressed
    ; Receives and decodes an IR signal from the remote and plays the appropriate tune
    ; Allows a custom tune to be programmed into EEPROM memory
; PIC used: Microchip PIC16F84A
    ; Datasheet available: http://ww1.microchip.com/downloads/en/DeviceDoc/35007C.pdf

; Configure environment for pic
            LIST p=16F84A
            include "p16f84a.inc"

; Remove the annoying "Register in operand not in bank 0..." warnings.
            errorlevel  -302

; Set up constants

    ; These are the constants used for notes in the tune tables.
    ; The less significant nibble is an encoded number representing the note.
        ; e.g. H'X0' is a C, H'X1' is a C#, H'X2' is a D etc.
    ; The more significant nibble is the TMR0 scaler (see datasheet). This sets the ocatve.
    ; This system works because the frequency of a note doubles in successive octaves
    ; Scaler value = 2; tmr0 scaling ratio = 1:8; this corresponds to the octave starting on C4 (middle C)
    ; Scaler value = 3; tmr0 scaling ratio = 1:16; this corresponds to the octave starting on C3
    ; etc.

C1          equ     H'50'
C#1         equ     H'51'
D1          equ     H'52'
D#1         equ     H'53'
E1          equ     H'54'
F1          equ     H'55'
F#1         equ     H'56'
G1          equ     H'57'
G#1         equ     H'58'
A1          equ     H'59'
A#1         equ     H'5A'
B1          equ     H'5B'

C2          equ     H'40'
C#2         equ     H'41'
D2          equ     H'42'
D#2         equ     H'43'
E2          equ     H'44'
F2          equ     H'45'
F#2         equ     H'46'
G2          equ     H'47'
G#2         equ     H'48'
A2          equ     H'49'
A#2         equ     H'4A'
B2          equ     H'4B'

C3          equ     H'30'
C#3         equ     H'31'
D3          equ     H'32'
D#3         equ     H'33'
E3          equ     H'34'
F3          equ     H'35'
F#3         equ     H'36'
G3          equ     H'37'
G#3         equ     H'38'
A3          equ     H'39'
A#3         equ     H'3A'
B3          equ     H'3B'

C4          equ     H'20'       ; Middle C
C#4         equ     H'21'
D4          equ     H'22'
D#4         equ     H'23'
E4          equ     H'24'
F4          equ     H'25'
F#4         equ     H'26'
G4          equ     H'27'
G#4         equ     H'28'
A4          equ     H'29'
A#4         equ     H'2A'
B4          equ     H'2B'

C5          equ     H'10'
C#5         equ     H'11'
D5          equ     H'12'
D#5         equ     H'13'
E5          equ     H'14'
F5          equ     H'15'
F#5         equ     H'16'
G5          equ     H'17'
G#5         equ     H'18'
A5          equ     H'19'
A#5         equ     H'1A'
B5          equ     H'1B'

C6          equ     H'00'
C#6         equ     H'01'
D6          equ     H'02'
D#6         equ     H'03'
E6          equ     H'04'
F6          equ     H'05'
F#6         equ     H'06'
G6          equ     H'07'
G#6         equ     H'08'
A6          equ     H'09'
A#6         equ     H'0A'
B6          equ     H'0B'

rest        equ     H'2C'   ; Rest nominally has the scaler of 2, in reality this value doesn't matter.

    ; These are the note durations, they are used in a counter in the inturrupt loop.
    ; They are set to be equal to a whole note (semibreve) at 120BPM
C_dur       equ     D'131'
C#_dur      equ     D'139'
D_dur       equ     D'147'
D#_dur      equ     D'156'
E_dur       equ     D'165'
F_dur       equ     D'175'
F#_dur      equ     D'185'
G_dur       equ     D'196'
G#_dur      equ     D'208'
A_dur       equ     D'220'
A#_dur      equ     D'233'
B_dur       equ     D'247'

rest_dur    equ     D'110' ; The rest duration is the same as an 'A'. This is explained in the interrupt loop.

finish_pit  equ     H'2C' ; finish "pitch". This doesn't matter (so long as it works in the look-up tables).
finish_dur  equ     H'00' ; finish "duration". playatune interprets a duration of zero as the end of the tune.


; Assign variables in RAM, H'0C' is the first General purpose SRAM register
            CBLOCK H'0C'
            intTemp         ; INTerrupt TEMPorary storage (used to preserve W when the interrupt is called)
            duration        ; Holds the duration of each note (see interrupt for details of how this works)
            durMult         ; DURation MULTiplier (see interrupt for details of how this works)
            pitch           ; Holds the initial TMR0 value for each note.
            tneLkpOfst      ; TuNE LooKuP OFfSeT (The lookup offset for the tune tables)
            tuneSelect      ; Holds the tune currently being played, see mainloop for details of the format
            tmr0Scaler      ; Holds the TMR0 scaler, to set the octave. See setpitch for details
            setPitchTemp    ; Temporary storage for the setpitch subroutine (used to preserve W)
            setDurMultTmp   ; setdurmult TeMPorary storage  (used to preserve W when setdurmult is called)
            outerLpCntr     ; These are loop counters used by delay6ms and delay3ms
            innerLpCntr
            delay3msTmp     ; This is used to preserve W when delay6ms and delay 3ms are called
            ENDC

; Setup configuration values for the microcontroller
            ; Watchdog timer -> off (stops device rebooting itself)
            ; Powerup timer -> on (impose small delay on startup to allow voltage rail to stabilise)
            __config _XT_OSC & _WDT_OFF & _PWRTE_ON

; Set up where instructions are stored
            org H'0000'   ; address 0: this is where the reset vector goes to.
            goto setup
            org H'0004'   ; address 4: this is where the interrupt vector goes to.
; Interrupt Routine
            movwf   intTemp         ; Preserve contents of working register
            movf    pitch,W         ; reset tmr0  initial count
            btfsc   STATUS,Z        ; Consider special case of a rest (i.e. 'pitch' = 0)
            movlw   D'114'          ; If 'pitch' is zero, put D'114' (the pitch of an A) into TMR0, gives a rest the same duration as other notes
            movwf   TMR0
            ; toggle output
            movf    pitch,W
            btfsc   STATUS,Z        ; If pitch is zero the note is a rest, so don't toggle the output
            goto    skipToggle
            movlw   B'00000001'     ; Toggle output (PORTA, Pin 0) by XORing this with PORTA
            xorwf   PORTA,F
            ; Note length handler, this makes the note last [durMult] * [duration] half cycles of the note.
skipToggle  decfsz  durMult,F       ; decrement durMult (DURation MULTiplier)
            goto    retFrmInt       ; RETurn From Interrupt
            call    setDurMult      ; if durmult is zero then reset durMult and decrement duration
            decfsz  duration,F      ; if duration is zero, the note is finished, so clear the interrupt flag.
            goto    retFrmInt
            bcf     INTCON, T0IE    ; disable TMR0 interrupt enable, this stops the interrupt from being called
retFrmInt   movf    intTemp,W       ; return previous contents of working register
            bcf     INTCON,T0IF     ; clear tmr0 overflow flag, this allows the interrupt to be called again (when  TMR0 interrupt enable is 1)
            retfie

; Lookup Table Functions
    ; The lookup table functions are placed here (near the beginning) to ensure that they do not cross a page boundry

            ; This function returns the initial TMR0 value for each note
lookupPitch addwf   PCL,F   ; Skip [W] instructions
            retlw   D'17'   ; C
            retlw   D'31'   ; C#
            retlw   D'43'   ; D
            retlw   D'55'   ; D#
            retlw   D'66'   ; E
            retlw   D'77'   ; F
            retlw   D'87'   ; F#
            retlw   D'97'   ; G
            retlw   D'106'  ; G#
            retlw   D'114'  ; A
            retlw   D'122'  ; A#
            retlw   D'129'  ; B
            retlw   D'0'    ; Rest

            ; This function decodes the binary word recieved by IR and converts it to a value used by mainLoop
irDecodeLkp addwf   PCL,F   ; Skip [W] instructions
            retlw   B'00000001' ; Tune 1
            retlw   B'00000010' ; Tune 2
            retlw   B'00000100' ; Tune 3
            retlw   B'00001000' ; Tune 4

            ; This function sets the multiplier for the duration.
            ; This is necessary because the duration is calculated by counting the number of times the interrupt is called.
            ; The same note in sequential octaves will have twice the frequency, so will call the interrupt twice as many times.
                ; This is why the sequential return values are double eachother
            ; The value used as a lookup offset is the tmr0Scaler
                ; This is 5 for the lowest ocave, 4 for the second to lowest etc.
setDurMult  movwf   setDurMultTmp   ; Preserve W
            rlf     tmr0Scaler,W
            addwf   PCL,F           ; Skip 2*[tmr0Scaler] instructions
            movlw   D'32'
            goto    RetSDurMult     ; RETurn from Setting DURMULT
            movlw   D'16'
            goto    RetSDurMult
            movlw   D'8'
            goto    RetSDurMult
            movlw   D'4'
            goto    RetSDurMult
            movlw   D'2'
            goto    RetSDurMult
            movlw   D'1'
RetSDurMult movwf   durMult         ; Store the lookedup value in durMult
            movf    setDurMultTmp,W ; Restore previous value of W
            return

; Tune tables
    ; These return encoded note values and durations.
    ; The durations can be made smaller by dividing the duration a sensible constant.
    ; To end a tune, you must return finish_pit as the final pitch and finish_dur as the final duration

            ; Happy Birthday to you
tune1       addwf   PCL,F
            retlw   C4
            retlw   C_dur /8    ; Quaver
            retlw   C4
            retlw   C_dur /8
            retlw   D4
            retlw   D_dur /4    ; Crotchet
            retlw   C4
            retlw   C_dur /4
            retlw   F4
            retlw   F_dur /4
            retlw   E4
            retlw   E_dur /2
            retlw   C4
            retlw   C_dur /8
            retlw   C4
            retlw   C_dur /8
            retlw   D4
            retlw   D_dur /4
            retlw   C4
            retlw   C_dur /4
            retlw   G4
            retlw   G_dur /4
            retlw   F4
            retlw   F_dur /2    ; Minim
            retlw   C4
            retlw   C_dur /8
            retlw   C4
            retlw   C_dur /8
            retlw   C5
            retlw   C_dur /4
            retlw   A4
            retlw   A_dur /4
            retlw   F4
            retlw   F_dur /4
            retlw   E4
            retlw   E_dur /4
            retlw   D4
            retlw   D_dur /4
            retlw   A#4
            retlw   A#_dur /8
            retlw   A#4
            retlw   A#_dur /8
            retlw   A4
            retlw   A_dur /4
            retlw   F4
            retlw   F_dur /4
            retlw   G4
            retlw   G_dur /4
            retlw   F4
            retlw   F_dur /2
            retlw   finish_pit
            retlw   finish_dur     ; end of tune

            ; Imperial march
tune2       addwf   PCL,F
            retlw   G3
            retlw   G_dur /4
            retlw   G3
            retlw   G_dur /4
            retlw   G3
            retlw   G_dur /4
            retlw   D#3
            retlw   D#_dur /8
            retlw   A#3
            retlw   A#_dur /8
            retlw   G3
            retlw   G_dur /4
            retlw   D#3
            retlw   D#_dur /8
            retlw   A#3
            retlw   A#_dur /8
            retlw   G3
            retlw   G_dur /4
            retlw   rest
            retlw   rest_dur /4
            retlw   D4
            retlw   D_dur /4
            retlw   D4
            retlw   D_dur /4
            retlw   D4
            retlw   D_dur /4
            retlw   D#4
            retlw   D#_dur /8
            retlw   A#3
            retlw   A#_dur /8
            retlw   F#3
            retlw   F#_dur /4
            retlw   D#3
            retlw   D#_dur /8
            retlw   A#3
            retlw   A#_dur /8
            retlw   G3
            retlw   G_dur /4
            retlw   finish_pit
            retlw   finish_dur     ; end of tune

            ; The entire range of notes
tune3       addwf   PCL,F
            retlw   C1
            retlw   C_dur /10
            retlw   D1
            retlw   D_dur /10
            retlw   E1
            retlw   E_dur /10
            retlw   F1
            retlw   F_dur /10
            retlw   G1
            retlw   G_dur /10
            retlw   A1
            retlw   A_dur /10
            retlw   B1
            retlw   B_dur /10
            retlw   C2
            retlw   C_dur /10
            retlw   D2
            retlw   D_dur /10
            retlw   E2
            retlw   E_dur /10
            retlw   F2
            retlw   F_dur /10
            retlw   G2
            retlw   G_dur /10
            retlw   A2
            retlw   A_dur /10
            retlw   B2
            retlw   B_dur /10
            retlw   C3
            retlw   C_dur /10
            retlw   D3
            retlw   D_dur /10
            retlw   E3
            retlw   E_dur /10
            retlw   F3
            retlw   F_dur /10
            retlw   G3
            retlw   G_dur /10
            retlw   A3
            retlw   A_dur /10
            retlw   B3
            retlw   B_dur /10
            retlw   C4
            retlw   C_dur /10
            retlw   D4
            retlw   D_dur /10
            retlw   E4
            retlw   E_dur /10
            retlw   F4
            retlw   F_dur /10
            retlw   G4
            retlw   G_dur /10
            retlw   A4
            retlw   A_dur /10
            retlw   B4
            retlw   B_dur /10
            retlw   C5
            retlw   C_dur /10
            retlw   D5
            retlw   D_dur /10
            retlw   E5
            retlw   E_dur /10
            retlw   F5
            retlw   F_dur /10
            retlw   G5
            retlw   G_dur /10
            retlw   A5
            retlw   A_dur /10
            retlw   B5
            retlw   B_dur /10
            retlw   C6
            retlw   C_dur /10
            retlw   D6
            retlw   D_dur /10
            retlw   E6
            retlw   E_dur /10
            retlw   F6
            retlw   F_dur /10
            retlw   G6
            retlw   G_dur /10
            retlw   A6
            retlw   A_dur /10
            retlw   B6
            retlw   B_dur /10
            retlw   finish_pit
            retlw   finish_dur ; end of tune

            ; This is the tune the user has programmed into EEPROM memory
tune4       call    eeRead  ; Read the pitch or duration from EEPROM memory
            return

; Setup

setup       clrf    PORTA       ; Initialise ports
            clrf    PORTB
            bsf     STATUS,RP0  ; select bank 1

            ; Setup ports
            movlw   B'11111111'
            ;       B'-------1'  IR input
            ;       B'1111111-'  Tune programming buttons
            movwf   TRISB
            movlw   B'11111110'
            ;       B'-------0'  Speaker output
            ;       B'---1111-'  Play tune buttons
            ;       B'111-----'  Unimplemented
            movwf   TRISA

            ; Set up TMR0
                ; set up option register
            movlw   B'10000010'
                    ;'1-------' Disable port B pull up resistors
                    ;'--0-----' Increment TMR0 on internal clock
                    ;'----0---' Assign prescaler to TMR0 not watchdog timer
                    ;'-----010' Set prescaler at a ratio of 1:8 (This is arbitary and will be set for the octave of the note)
                    ;'-0-0----' Irrelevant options
            movwf   OPTION_REG
            ; Set up interrupt configuration register
            movlw   B'10000000'
                    ;'1-------' Global interrupt enable ON
                    ;'--0-----' TMR0 Overflow interrupt enable OFF (We will turn this on when we want it)
                    ;'-----0--' TMR0 Flag bit OFF (The interupt will not trigger if this is initially ON)
                    ;'-0-00-00' Turn OFF all other (unused) interrupts and flag bits
            movwf   INTCON

            ; Set up EEPROM
            clrf    EECON1      ; Clear all flags and read/write enable bits
            bcf     STATUS,RP0  ; Return to bank 0
            clrf    EEADR       ; Reset Read/write address to 0
            clrf    EEDATA

            ; Initialise various registers used inside the program
            clrf    tneLkpOfst  ; This initialises the lookup table offset, used in all the tunetables

; Start of the main program
            ; Loops waiting for a button press, or an IR signal
mainLoop    clrf    EEADR       ; Clear the EEADR register in case we need to program or read
            ; Look for a programming button press
            movf    PORTB,W
            andlw   B'11111110' ; Mask off IR LED input
            btfss   STATUS,Z
            call    progTune    ; If any of the programming buttons were pressed, call progtune
            clrw
            ; Look for an IR signal
            btfss   PORTB,0
            call    processIr   ; If an IR signal is being recieved, process it.
            ; Look for a tune button press
            btfsc   PORTA,1
            movlw   B'00000010' ;Set a status bit high if a tune button is presses - the order is chosen the order of the switches in the box!!
            btfsc   PORTA,2
            movlw   B'00000100'
            btfsc   PORTA,3
            movlw   B'00000001'
            btfsc   PORTA,4
            movlw   B'00001000'
            andlw   B'11111111' ; if w is zero, loop
            btfsc   STATUS,Z
            goto    mainLoop
            movwf   tuneSelect ; If switch is pressed, store which one was pressed in tuneSelect
            call    playATune
            goto    mainLoop

            ; Process IR signal
            ; For a valid signal, leave with W in the same status bit format as expected by mainloop
            ; For an invalid signal, leave with W clear
            ; Note the recieved signal with be the inverse of the transmitted signal
                ; Therefore the expected word recieved is: 0, NOT(Data1), NOT(Data2), 1, 1
                ; See remote_main.asm for exact details of the data word
processIr   clrw
            call    delay3ms    ; Delay 3ms to get to the middle of the first pulse
            btfsc   PORTB,0     ; Ensure the recieved start bit is a 0
            goto    irFailure
            call    delay6ms    ; Wait for next bit
            btfss   PORTB,0     ; If the line is low, set Data1 to 1
            iorlw   B'00000001'
            call    delay6ms
            btfss   PORTB,0     ; If the line is low, set Data2 to 1
            iorlw   B'00000010'
            call    delay6ms
            btfss   PORTB,0     ; Ensure the two stop bits are 1
            goto    irFailure
            call    delay6ms
            btfss   PORTB,0
            goto    irFailure
            call    irDecodeLkp ; Decode W to the word expected by mainLoop
            return
irFailure   clrw                ; If there was a problem with the word, clear W and exit
            return

; Delay 6ms and delay 3ms
    ; Each instruction takes 4 clock cycles = 1us
    ; The inner loop takes 5 cycles (assuming it doesn't skip) = 5us
        ; Therefore 200 iterations takes 1ms
    ; The outer loop allows the inner loop to run 3 or 6 times
        ; This takes 3 or 6ms
delay6ms    movwf   delay3msTmp     ; Preserve W
            movlw   D'6'
            movwf   outerLpCntr
            goto    delayLoop
delay3ms    movwf   delay3msTmp     ; Preserve W
            movlw   D'3'
            movwf   outerLpCntr
delayLoop   movlw   D'200'
            movwf   innerLpCntr
innerLoop   nop
            decfsz  innerLpCntr,F
            goto    innerLoop
outerLoop   decfsz  outerLpCntr,F
            goto    delayLoop
            movf    delay3msTmp,W   ; Restore W
            return

            ; This subroutine reads the user input from the programming keys and saves it as a custom tune in EEPROM
progTune    btfss   PORTB,1     ; test if button 1 is pressed, if so program in a C, if not check the next button
            goto    prog2
            movlw   C4          ; C4 in pitch
            movwf   pitch
            movlw   C_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe      ; Save to EEPROM and play note once
            goto    progSkp     ; All the other buttons must be unpressed so skip to the end
prog2       btfss   PORTB,2     ; Repeat for remaining buttons
            goto    prog3
            movlw   D4          ; D4 in pitch
            movwf   pitch
            movlw   D_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe
            goto    progSkp
prog3       btfss   PORTB,3
            goto    prog4
            movlw   E4          ; E4 in pitch
            movwf   pitch
            movlw   E_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe
            goto    progSkp
prog4       btfss   PORTB,4
            goto    prog5
            movlw   F4          ; F4 in pitch
            movwf   pitch
            movlw   F_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe
            goto    progSkp
prog5       btfss   PORTB,5
            goto    prog6
            movlw   G4          ; G4 in pitch
            movwf   pitch
            movlw   G_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe
            goto    progSkp
prog6       btfss   PORTB,6
            goto    prog7
            movlw   A4          ; A4 in pitch
            movwf   pitch
            movlw   A_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe
            goto    progSkp
prog7       btfss   PORTB,7
            goto    progSkp
            movlw   B4          ; C4 in pitch
            movwf   pitch
            movlw   B_dur /4    ;Crotchet in duration
            movwf   duration
            call    progEe
progSkp     movlw   H'3D'       ; Make sure next address is still at least two bytes before the end of the EEPROM memory
            subwf   EEADR,W
            btfsc   STATUS,C    ; If the C flag is high, the result was negative. so we're still in the acceptable range
            goto    retProgTune ; (RETurn from ProgTune) If we are at the end of the memory, exit.
            movf    PORTA,W     ; Test if any tune buttons are being pressed, if so we're done programming
            andlw   B'00011110' ; Mask off all pins that aren't the buttons
            btfsc   STATUS,Z
            goto    progTune    ; if a button wasn't pressed loop for the next note
retProgTune movlw   finish_pit  ; Write the finish pitch and duration
            movwf   pitch
            movlw   finish_dur
            movwf   duration
            call    progEe      ; Save to EEPROM
            clrf    EEADR       ; clear EEADR for next read or write
            return

            ; These functions programs the note in pitch and duration to EEPROM memory.
                ; It also plays the note to give the user feedback.
progEe      movf    pitch,W     ; Write the pitch
            call    eeWrite
            incf    EEADR,F     ; Increment the EEPROM address
            movf    duration,W  ; Write the duration
            call    eeWrite
            incf    EEADR,F     ; Increment the EEPROM address
            movf    duration,F  ; If duration = 0, we're programming the finish command so skip playAnote
            btfss   STATUS,Z
            call    playANote   ; Play the note
            clrw
            return

            ; This function plays a single note from a coded pitch value in pitch and a duration value in duration
playANote   movf    pitch,W
            call    setPitch        ; Decode pitch
            bsf     INTCON, T0IE    ; set the TMR0 Interrupt Enable bit, this will allow the interrupt to play the note
noteLoop    btfsc   INTCON, T0IE    ; Wait for note to finish, then return
            goto    noteLoop
            return

            ; This subroutine is responsible for playing a tune
                ; It loops until the tune is over
playATune   movf    tneLkpOfst,W
            call    getTuneData     ; Look up the next pitch in the tune
            movwf   pitch
            call    getTuneData     ; Lookup next duration
            movwf   duration
            movf    duration,F      ; test if duration is zero
            btfss   STATUS,Z        ; if zero, the tune is over
            goto    skpPATRet       ; Therefore if not zero, skip this next section (skppatret = SKiP PlayATune RETurn)
            clrf    tneLkpOfst      ; Clear the lookup offset and return
            return
skpPATRet   call    playANote       ; Play the note
            movlw   rest            ; Play a short rest to make the tune sound more natural
            movwf   pitch
            movlw   rest_dur /20
            movwf   duration
            call    playANote
            goto    playATune       ; If the note is over, go back to the start of the subroutine to play the next note

            ; This subroutine directs the program to the relevant tune table.
                ; See 'mainloop' for details of the value in tuneSelect
                ; NOTE: It is important that this function does NOT change the W register.
                ; This is because W contains either the lookup offset (on call) or the table return value (on return).
getTuneData movf    tneLkpOfst,W
            btfsc   tuneSelect,0    ; If the LSB is set we want tune1
            call    tune1
            btfsc   tuneSelect,1    ; If the bit 1 is set we want tune2
            call    tune2
            btfsc   tuneSelect,2    ; If the bit 2 is set we want tune3
            call    tune3
            btfsc   tuneSelect,3    ; If the bit 3 is set we want tune4
            call    tune4
            incf    tneLkpOfst,F
            return

            ; This subroutine:
                ; decodes the pitch value returned by the tune tables into the start value of the TMR0 register
                ; sets the TMR0 scaler
                ; sets the duration multiplier
            ; The value returned by the tune table is passed to setPitch in W.
                ; The upper nibble contains the TMR0 scaler (see datasheet)
                ; The lower nibble contains a lookup offset for the TMR0 start value (lookupPitch function for more details)
setPitch    movwf   setPitchTemp    ; store intact working register to preserve lookup offset
            andlw   B'01110000'     ; mask off the TMR0 scaler bits
            movwf   tmr0Scaler
            rrf     tmr0Scaler,F    ; Shift the scaler bits to the same place as they are in in OPTION_REG (The least significant three bits)
            rrf     tmr0Scaler,F
            rrf     tmr0Scaler,F
            rrf     tmr0Scaler,F
            bsf     STATUS,RP0      ; select bank 1 to access OPTION_REG
            movf    OPTION_REG,W
            andlw   B'11111000'     ; Mask off unrelated bits to presrve them
            iorwf   tmr0Scaler,W    ; Copy scaler bits to W
            movwf   OPTION_REG      ; Save back to OPTION_REG
            bcf     STATUS,RP0      ; Return to bank 0
            movlw   B'00001111'     ; Mask off the part the TMR0 start value lookup offset
            andwf   setPitchTemp,W
            call    lookupPitch     ; Lookup the TMR0 start value for that note
            movwf   pitch           ; Put this in "pitch"
            call    setDurMult      ; Setup the duraion multiplier so that the interrupt timing is correct
            return

            ; This subroutine writes the data in W to the address in EEADR
eeWrite     movwf   EEDATA          ; Move W to EEDATA
            bsf     STATUS,RP0      ; Select Bank 1
            bcf     INTCON,GIE      ; Disable interrupts
            bsf     EECON1,WREN     ; Enable EEPROM write
            ; Perform prescribed write sequence (see datasheet)
            movlw   H'55'
            movwf   EECON2
            movlw   H'AA'
            movwf   EECON2
            bsf     EECON1, WR      ; Begin Write
            ; End prescribed write sequence
            bsf     INTCON, GIE     ; re-enable interrupts
            ; Wait for bit to be written
eeWriteLoop btfss   EECON1,EEIF     ; Test write operation interrupt flag
            goto    eeWriteLoop     ; Loop until write completes
            bcf     EECON1,EEIF     ; Clear interrupt flag
            bcf     STATUS,RP0      ; Select Bank 0
            return

            ; This subroutine reads the value of the data in the EEPROM address corresponding to the value of W, and returns this value in W
eeRead      movwf   EEADR           ; Move address to EEADR
            bsf     STATUS,RP0      ; Bank 1
            bsf     EECON1,RD       ; Read Value to EEDATA
            bcf     STATUS,RP0      ; Bank 0
            movf    EEDATA,W
            return

            end