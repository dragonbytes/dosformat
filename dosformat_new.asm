*****************************************************
* DOSFORMAT v1.5 Written by Todd Wallace
* A formatting tool for NitrOS-9 to make MSDOS FAT12 
* floppy disks using a real CoCo.
*****************************************************

; Definitions/equates 
sector_length 	EQU 	2 		; 2 = 512 bytes 
;skip_format 	EQU 	1

STDOUT          EQU   1
STDIN 			EQU   0
H6309    		set   0

	include 	os9.d
	include 	rbf.d
	include 	scf.d 

	pragma 	cescapes

; Module header setup info 
	MOD 	MODULE_SIZE,moduleName,$11,$80,START_EXEC,data_size

START_MODULE
**************************************************************************************
; -----------------------------------------------------
; Variables 
		org 	0

uRegImage 			RMB 	2
strHexWord 			RMB 	6
u32Value 			RMB 	4
u16Value 			RMB 	2
u8Value 			RMB 	1
asciiByteDecLen 	RMB 	1
u16strLength 		RMB 	1
u32strLength 		RMB 	1
temp 				RMB 	14
fatCounter 			RMB 	1
drivePath 			RMB 	1
prevEKO 			RMB 	1
percentComplete 	RMB 	1
totalDiskTracks 	RMB 	2
verifyFlag 			RMB 	1

;preIndexGapPtr 		RMB 	2
;idFieldPrefixPtr 	RMB 	2
;sectorBodyPtr 		RMB 	2
;finalGapPtr 		RMB 	2
rawTrackBufferStart RMB 	2
stackBlastStartPtr 	RMB 	2
rootDirEndPtr 		RMB 	2
paramPtr 			RMB 	2
origParamPtr 		RMB  	2
bootSectorPtr  		RMb  	2

track 				RMB 	1
side 				RMB 	1
sector 				RMB 	1
strPath 			RMB 	6
allocSize 			RMB 	2
totalBytes 			RMB 	4 
strAsciiU32 		RMB 	10
volumeID 			RMB 	4

verifyStepValue 	RMB  	1 
verifyCounter 		RMB  	1 
verifyRemainder 	RMB  	1 
verifyRemCounter 	RMB  	1 
verifySectorCounter RMB  	2

tracksPerSide  		RMB  	2
bytesPerSector  	RMB  	2
sectsPerCluster  	RMB  	1
fatTableCount		RMB 	1 	 
maxRootDirNum		RMB 	2
totalSectors		RMB 	2
sectorsPerFAT		RMB 	2 
numSides			RMB 	2
fatHeader  			RMB  	2
os9driveTYP  		RMB  	1
os9driveDNS  		RMB  	1
os9driveSCT  		RMB  	2
os9driveILV  		RMB  	1

strBuffer 			RMB 	128
statBuffer 			RMB 	32 
stdinPD 			RMB 	32 
volumeName 			RMB 	12 			; 11 chars plus CR 
paddedVolumeName 	RMB 	12 			; 11 chars plus NULL 
dateTimeData 		RMB 	6 			
rawTrackBuffer 		RMB 	6500

					RMB 	2048 		; add 2k of padding so os9 allocated 2 blocks 
										; and theres room for the stack 
; End of Variables
; -----------------------------------------------------
data_size    	EQU   .
; -----------------------------------------------------
; Constants
moduleName 		FCS 	"dosformat"
;interleaveArray FCB	1,6,2,7,3,8,4,9,5,$FF 	; $FF marks end of sector list (2:1)
;interleaveArray FCB 	1,8,6,4,2,9,7,5,3,$FF	; $FF marks end of sector list (4:1)
interleaveArray	FCB 	1,2,3,4,5,6,7,8,9,$FF 	; $FF marks end of sector list (1:1)
; this is the raw track structure. first byte is what is to be written, second how many times.	
preIndexGap		FCB 	$4E 		; Gap 5
		 		FCB 	80
		 		FCB 	$00
			 	FCB 	12
			 	FCB 	$F6 		; Writes $C2 byte 
			 	FCB 	3

				FCB 	$FC 		; Index AM 
				FCB 	1
 						
				FCB 	$4E 		; Gap 1
				FCB 	50 			; from fdc controller spec 
				;FCB 	32 			; from os9 FORMAT command 	
				FCB 	$00
				FCB 	12
				FCB 	$F5 		; generates $A1 byte 
				FCB 	3

				FCB 	$FE 		; ID AM 
				FCB 	1
				FDB 	$0000 		; marks end of sequence for program 

idFieldPrefix	FCB 	$4E 		; Gap 3
				FCB 	$54
				;FCB  	54
				FCB 	$00
				FCB 	12
				FCB 	$F5
				FCB 	3

				FCB 	$FE 		; ID AM 
				FCB 	1
				FDB 	$0000 		; end marker 

sectorBody		FCB 	$4E 		; Gap 2
				FCB 	22
				FCB 	$00
				FCB 	12
				FCB 	$F5 		; generates $A1 byte 
				FCB 	3

				FCB 	$FB 		; Data AM 
				FCB 	1
				; 2 of these in a row should total 512 bytes 
				FCB 	$E5 		; blank byte value on disk 
				FCB 	256
				FCB 	$E5 		; blank byte value on disk 
				FCB 	256

				FCB 	$F7 		; generate 2 byte CRC
				FCB 	1
				FDB 	$0000 		; terminator 

finalGap		FCB 	$4E 		; Gap 4 (652 bytes of $4E)
				FCB 	256
				FCB 	$4E
				FCB 	256
			;	FCB 	$4E
		;		FCB 	240
				FDB 	$0000

os9driveConfig720 	;FCB  	$45  				; PD.TYP to set disk type
					FCB  	%01000101
					FCB   	%00000101			; PD.DNS to set track single density and double bit density
					FDB  	80  				; PD.CYL to set number of tracks per side
					FDB  	9 					; PD.SCT and PD.T0S set to 9 sectors per track
					FCB   	1  					; PD.ILV interleave set to 1
; FAT12 data (NOTE: ALL VALUES ARE INTEL LITTLE INDIAN)
bootSectorStart720 	FCB 	$EB,$3C,$90 		; Dummy jump instruction 
					FCC 	"MSDOS5.0" 			; OEM name 
					FDB 	$0002 				; 512 bytes per sector 
					FCB 	$02 				; 2 logical sectors per cluster 
					FDB 	$0100 				; 1 reserved logical sector (boot sector)
					FCB 	$02 				; 2 file allocation tables 
					FDB 	$7000 				; number of sectors for root directory entries (112 maximum)
					FDB 	$A005 				; 1440 total 512 byte logical sectors on disk
					FCB 	$F9 				; media descriptor for 3.5" 80trk/9sec DS 
					FDB 	$0300 				; 3 logical sectors per FAT 
					FDB 	$0900 				; 9 sectors per track 
					FDB 	$0200 				; 2 heads (sides)
					FQB 	0 					; number of hidden sectors (quad byte)
					FQB 	0 					; number of "large" sectors (>65535) (quad byte)
					FCB  	$00  				; physical drive number
					FCB  	$00  				; flags and such
					FCB 	$29 				; Extended boot signature
;volumeID		FDB 	$0963,$0968 		; volume ID (6809-6309 for fun)
bootSectorStartSz 	EQU 	*-bootSectorStart720  	; this constant is the same for any disk type so reuse it
bootSectorExtParam
				includebin ext_bios_param_block.bin 
bootSectorExtParamSz EQU *-bootSectorExtParam
; +11 is for the volume name, +4 for the volume ID 
bootSectorRemainder EQU 512-(*-bootSectorStart720+11+4) 

os9driveConfig360 	FCB  	$44  				; PD.TYP to set disk type
					FCB   	%00000101			; PD.DNS to set track single density and double bit density
					FDB  	40  				; PD.CYL to set number of tracks per side
					FDB  	9 					; PD.SCT and PD.T0S set to 9 sectors per track
					FCB   	1  					; PD.ILV interleave set to 1
bootSectorStart360 	FCB  	$EB,$3C,$90 		; Dummy jump instruction 
					FCC 	"MSDOS5.0" 			; OEM name 
					FDB 	$0002 				; 512 bytes per sector 
					FCB 	$02 				; 2 logical sectors per cluster 
					FDB 	$0100 				; 1 reserved logical sector (boot sector)
					FCB 	$02 				; 2 file allocation tables 
					FDB 	$7000 				; number of sectors for root directory entries (112 maximum)
					FDB 	$D002 				; 720 total 512 byte logical sectors on disk
					FCB 	$FD 				; media descriptor for 5.25" 40trk/9sec DS 
					FDB 	$0200 				; 2 logical sectors per FAT 
					FDB 	$0900 				; 9 sectors per track 
					FDB 	$0200 				; 2 heads (sides)
					FQB 	0 					; number of hidden sectors (quad byte)
					FQB 	0 					; number of "large" sectors (>65535) (quad byte)
					FCB  	$00  				; physical drive number
					FCB  	$00  				; flags and such
					FCB 	$29 				; Extended boot signature
;volumeID			FDB 	$0963,$0968 		; volume ID (6809-6309 for fun)

os9driveConfig180 	FCB  	$44  				; PD.TYP to set disk type
					FCB   	%00000101			; PD.DNS to set track single density and double bit density
					FDB  	40  				; PD.CYL to set number of tracks per side
					FDB  	9 					; PD.SCT and PD.T0S set to 9 sectors per track
					FCB   	1  					; PD.ILV interleave set to 1
bootSectorStart180 	FCB  	$EB,$3C,$90 		; Dummy jump instruction 
					FCC 	"MSDOS5.0" 			; OEM name 
					FDB 	$0002 				; 512 bytes per sector 
					FCB 	$01 				; 1 logical sector per cluster 
					FDB 	$0100 				; 1 reserved logical sector (boot sector)
					FCB 	$02 				; 2 file allocation tables 
					FDB 	$4000 				; number of sectors for root directory entries (64 maximum)
					FDB 	$6801 				; 360 total 512 byte logical sectors on disk
					FCB 	$FC 				; media descriptor for 5.25" 40trk/9sec single-sided 
					FDB 	$0200 				; 2 logical sectors per FAT 
					FDB 	$0900 				; 9 sectors per track 
					FDB 	$0100 				; 2 heads (sides)
					FQB 	0 					; number of hidden sectors (quad byte)
					FQB 	0 					; number of "large" sectors (>65535) (quad byte)
					FCB  	$00  				; physical drive number
					FCB  	$00  				; flags and such
					FCB 	$29 				; Extended boot signature

; for 32 bit math 
bin32dec1B 			FQB 	1000000000 	; 1 billion decimal 
bin32dec100M 		FQB 	100000000 	; 100 million decimal 
bin32dec10M 		FQB 	10000000 	; 10 million decimal 
bin32dec1M 			FQB 	1000000 		; 1 million decimal 
bin32dec100K 		FQB 	100000 		; 100 thousand decimal 
bin32dec10K 		FQB 	10000 	

strNoName 			FCN 	"NO NAME    "

strIntro 			FCN 	"dosformat v1.5 - Written by Todd Wallace (LordDragon)\r\n\n"

strUsage 			FCC 	"This standalone tool can format a raw/blank floppy disk for MS-DOS FAT12\r\n"
					FCC 	"without the need of any 3rd party drivers.\r\n\n"
		 			FCC 	"Usage: dosformat <drive> [-v] [-f:<size>]\r\n\n   -v = Verify the format afterwards.\r\n"
		 			FCN  	"   -f = Format disk size. Valid sizes are 720k and 360k. (Defaults to 720k)\r\n"

strNewDiskPrompt	FCN 	"Insert new diskette for drive "
strEnterPrompt 		FCN 	"\r\nand press ENTER when ready..."

;strChecking 		FCN 	"Checking existing disk format.\r\n"
strFormatting 		FCN 	"\r\n\nFormatting "

strPercentCompleted	FCN 	" percent completed."
strFormatComplete 	FCN 	"\x03\rFormat complete.\r\n\n"
strVolumePrompt 	FCN 	"Volume label (11 characters, ENTER for none)? "

strDiskSpace 		FCN 	" bytes total disk space\r\n"
strAvailSpace		FCN 	" bytes available on disk\r\n\n"
strBytesPerAlloc 	FCN 	" bytes in each allocation unit.\r\n"
strAllocUnitsAvail 	FCN 	" allocation units available on disk.\r\n\n"
strVolumeSerial 	FCN 	"Volume Serial Number is "
strVerifying 		FCN 	"\x03\x09\x03\rVerifying "

strErrorParams 		FCN 	"Invalid parameter. Usage: dosformat <drive> [-v] [-f:<size>]\r\n"
strErrorMissingDrv 	FCN  	"Missing drive parameter. Usage: dosformat <drive> [-v] [-f:<size>]\r\n"
strErrorDiskType  	FCN  	"Invalid format size. Valid sizes are 720k and 360k.\r\n"
strErrorPath 		FCN 	"Could not open path to "
strErrorDiskRaw 	FCN 	"\r\nAn error while trying to low-level format disk.\r\n"
strErrorDiskSector 	FCN 	"\r\nError reading/writing to sectors on formatted disk. Aborted.\r\n"
strErrorVerify 		FCN 	"\r\n\nError while verifying the format. Current position is below:\r\n"

strError 		FCC 	"Something went wrong...\r"
asciiHexList	FCC 	"0123456789ABCDEF"

; -----------------------------------------------------

START_EXEC
**************************************************************************************
* Program code area 
* RULE #1 - USE U TO REFERENCE ANY CHANGEABLE VARIABLES IN THE DATA AREA.
* RULE #2 - USE PCR TO REFERENCE CONSTANTS SINCE THEY RESIDE WITH EXECUTABLE CODE.
* RULE #3 - NEVER USE JSR FOR CALLING SUBROUTINES. ALWAYS USE BSR OR LBSR INSTEAD.
**************************************************************************************

	stu   	<uRegImage        ; save copy of data area pointer in U 
	stx 	<paramPtr 
	stx  	<origParamPtr

	; check for no params error condition first 
	lda 	,X 
	cmpa 	#C$CR 
	bne 	PARAMS_NOT_BLANK

	; show usage info and exit 
	leax 	strIntro,PCR 
	lbsr  	PRINT_NULL_STR
	leax   	strUsage,PCR 
	lbsr  	PRINT_NULL_STR
	lbra 	EXIT_ONLY

PARAMS_NOT_BLANK
	; make 720k 3.5 inch disks the default if no format flags are set 
	leay   	bootSectorStart720,PCR 
	lbsr   	POPULATE_DISK_SETTINGS

	leax 	strIntro,PCR 
	lbsr  	PRINT_NULL_STR

	; verify some drive destination is given for first param
	ldx 	<paramPtr
	lda 	,X
	cmpa  	#'/'
	lbne  	ERROR_MISSING_DRIVE_PARAM
	; seems so. try and open path to drive 
	leay 	strPath,U 
	lbsr 	PARAM_COPY
	stx 	<paramPtr 		; update the pointer 
	lda 	#'@'
	ldb 	#C$CR
	std 	,Y 			; append '@' to end of drive name for raw access 
	lda 	#UPDAT.
	leax 	strPath,U 
	os9 	I$Open 
	bcc 	RAW_PATH_OPENED
	; report error opening path 
	leay 	strBuffer,U 
	leax 	strErrorPath,PCR 
	lbsr 	STRING_COPY_RAW
	ldx 	<origParamPtr
	lbsr 	PARAM_COPY
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR
	lbra 	EXIT_ONLY  

RAW_PATH_OPENED
	sta 	<drivePath 

	; check for other parameters in command line 
	ldx 	<paramPtr
PARAMS_GET_NEXT
	lbsr 	SEARCH_PARAMETER_FLAG
	bcs 	FINISHED_ALL_PARAMS
	lda 	,X+ 
	lbsr 	CONVERT_UPPERCASE
	cmpa  	#'F'
	beq   	PARAMS_CHECK_DISK_TYPE
	cmpa 	#'V'
	lbne 	ERROR_INVALID_PARAM
	inc 	<verifyFlag 
	bra 	PARAMS_GET_NEXT

PARAMS_CHECK_DISK_TYPE
	lda  	,X+
	cmpa  	#':'
	lbne 	ERROR_INVALID_PARAM
	ldd  	,X++
	cmpd  	#"72"
	bne  	PARAMS_CHECK_DISK_TYPE_NOT_720
	lda  	,X+
	cmpa  	#'0'
	lbne  	ERROR_INVALID_DISK_TYPE
PARAMS_SET_720K
	leay   	bootSectorStart720,PCR 
	lbsr   	POPULATE_DISK_SETTINGS
	lbsr   	FIND_NEXT_SPACE_NULL_CR
	bra 	PARAMS_GET_NEXT

PARAMS_CHECK_DISK_TYPE_NOT_720
	cmpd  	#"36"
	bne  	PARAMS_CHECK_DISK_NOT_360
	lda  	,X+
	cmpa  	#'0'
	lbne  	ERROR_INVALID_DISK_TYPE
PARAMS_SET_360K
	leay  	bootSectorStart360,PCR 
	lbsr  	POPULATE_DISK_SETTINGS
	lbsr   	FIND_NEXT_SPACE_NULL_CR
	bra 	PARAMS_GET_NEXT

PARAMS_CHECK_DISK_NOT_360
	cmpd  	#"18"
	lbne  	ERROR_INVALID_DISK_TYPE
	lda  	,X+
	cmpa  	#'0'
	lbne  	ERROR_INVALID_DISK_TYPE
PARAMS_SET_180K
	leay   	bootSectorStart180,PCR 
	lbsr  	POPULATE_DISK_SETTINGS
	lbsr   	FIND_NEXT_SPACE_NULL_CR
	bra 	PARAMS_GET_NEXT

FINISHED_ALL_PARAMS
	; init some variables 
	clra 
	sta		<track
	sta 	<side 
	sta 	<verifyFlag
	;leax 	preIndexGap,PCR 
	;stx 	<preIndexGapPtr
	;leax 	idFieldPrefix,PCR 
	;stx 	<idFieldPrefixPtr	
	;leax 	sectorBody,PCR
	;stx 	<sectorBodyPtr	
	;leax 	finalGap,PCR 
	;stx 	<finalGapPtr

	lda 	numSides,U 
	ldb 	tracksPerSide+1,U 
	mul 
	std 	totalDiskTracks,U 	

	; get current STDIN SCF setup 
	lda 	#STDIN 
	ldb 	#SS.Opt 
	leax 	stdinPD,U 
	os9 	I$GetStt
	; preserve original echo value, turn echo off, and write back settings 
	ldb 	PD.EKO-PD.OPT,X
	stb 	<prevEKO
	clr 	PD.EKO-PD.OPT,X 
	lda 	#STDIN
	ldb 	#SS.Opt 
	leax 	stdinPD,U 
	os9 	I$SetStt 

	; setup OS9 floppy drive settings for 512 bytes per sector and 9 sectors per track 
	lda 	<drivePath
	ldb 	#SS.Opt 
	leax 	statBuffer,U
	os9 	I$GetStt
	ldb 	<os9driveTYP
	stb 	PD.TYP-PD.OPT,X  			; set TYP 
	ldb 	<os9driveDNS
	stb 	PD.DNS-PD.OPT,X  			; double density setting for 135 tpi 
	ldd  	<tracksPerSide
	std   	PD.CYL-PD.OPT,X   			; tracks (cylinders) per side
	ldb  	<numSides
	stb  	PD.SID-PD.OPT,X  			; number of total sides per disk
	ldd 	<os9driveSCT
	std 	PD.SCT-PD.OPT,X  
	std 	PD.T0S-PD.OPT,X 
	ldb 	<os9driveILV
	stb 	PD.ILV-PD.OPT,X  			; interleave value of 1 
	; write new setup to PD 
	lda 	<drivePath 
	ldb 	#SS.Opt 
	leax 	statBuffer,U 
	os9 	I$SetStt 

	leay 	strBuffer,U 
	leax 	strNewDiskPrompt,PCR 
	lbsr 	STRING_COPY_RAW
	; get the name of drive in path 
	lda 	<drivePath
	ldb 	#SS.DevNm 
	leax 	statBuffer,U 
	os9 	I$GetStt
	; copy the name result to output buffer 
	lda 	#'/'
	sta 	,Y+
	clrb 
DRIVE_DESC_NAME_NEXT
	lda 	,X+
	bmi 	DRIVE_DESC_NAME_END 	
	sta 	,Y+
	decb 
	bne 	DRIVE_DESC_NAME_NEXT
DRIVE_DESC_NAME_END
	anda 	#%01111111
	clrb 
	std 	,Y
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	leax 	strEnterPrompt,PCR 
	lbsr  	PRINT_NULL_STR

WAIT_FOR_ENTER
	lda 	#STDIN
	leax 	temp,U 
	ldy 	#1
	os9 	I$Read 
	ldb 	temp,U 
	cmpb 	#C$CR 
	bne 	WAIT_FOR_ENTER
 
	; reenable SCF echo so we can prompt for volume name later 
	leax 	stdinPD,U 
	ldb 	#1
	stb 	PD.EKO-PD.OPT,X 		; enable echo again 
	lda 	#STDIN
	ldb 	#SS.Opt 
	os9 	I$SetStt 

	leay 	strBuffer,U 
	leax 	strFormatting,PCR
	lbsr 	STRING_COPY_RAW

	ldb 	totalSectors,U 
	lda 	totalSectors+1,U
	lsra 
	rorb  
	lbsr 	CONVERT_BINARY16_DECIMAL
	lda 	#'K'
	sta 	,Y+
	ldd 	#$0D0A 
	std 	,Y++	
	clr 	,Y 
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	; now we build a raw track template to use during the format 
	leay 	rawTrackBuffer,U 
	sty 	<rawTrackBufferStart
	leau 	interleaveArray,PCR 
	leax  	preIndexGap,PCR 
	ldd 	,X++
PREINDEX_LOOP_NEXT
	sta 	,Y+
	decb 
	bne 	PREINDEX_LOOP_NEXT
	; get next part
	ldd 	,X++
	bne 	PREINDEX_LOOP_NEXT

ADD_NEXT_SECTOR
	; now add the track, side, sector information 
	ldd 	<track  		; track in A and side in B 
	std 	,Y++
	lda 	,U+
	ldb 	#sector_length
	std 	,Y++
	lda 	#$F7 			; generate 2 byte CRC after ID field 
	sta 	,Y+

	; now add the next gap and sector data areas 
	leax  	sectorBody,PCR 
	ldd 	,X++
BODY_LOOP_NEXT
	sta 	,Y+
	decb 
	bne 	BODY_LOOP_NEXT
	; get next part
	ldd 	,X++
	bne 	BODY_LOOP_NEXT

	; was this the last sector?
	lda 	,U 
	bmi 	FINAL_GAP

	; add a Gap 3 and branch to populate next sector 
	leax  	idFieldPrefix,PCR 
	ldd 	,X++
PRE_ID_LOOP_NEXT
	sta 	,Y+
	decb 
	bne 	PRE_ID_LOOP_NEXT
	; get next part
	ldd 	,X++
	bne 	PRE_ID_LOOP_NEXT
	bra 	ADD_NEXT_SECTOR

FINAL_GAP
	leax  	finalGap,PCR 
	ldd 	,X++
FINAL_GAP_LOOP
	sta 	,Y+
	decb 
	bne 	FINAL_GAP_LOOP
	ldd 	,X++
	bne 	FINAL_GAP_LOOP

	ldu 	<uRegImage

 IFNDEF 	skip_format
	; reset to floppy drive head to track 0 in prepartion for writing raw tracks 
	lda 	<drivePath
	ldb 	#SS.Reset
	os9 	I$SetStt 
	lbcs 	ERROR_DISK_ACCESS
 ENDC

FORMAT_TRACK_NEXT
	; first calculate current percentage of progress 
	ldb 	tracksPerSide+1,U 
	lsrb 	
	stb 	<u8Value 		; used to check for rounding later 
	clr 	<percentComplete
	ldb 	<track 
	incb 					; for percent calc, we need to start with 1 instead of 0 
	lda 	#100
	mul 
FORMAT_TRACK_PERCENT_DIVIDE
	subd 	tracksPerSide,U 
	bcs 	FORMAT_TRACK_PERCENT_DIVIDE_DONE
	inc 	<percentComplete
	bra 	FORMAT_TRACK_PERCENT_DIVIDE
FORMAT_TRACK_PERCENT_DIVIDE_DONE
	addd 	tracksPerSide,U 		; restore remainder 
	cmpb 	<u8Value				; should we round up?
	blo 	FORMAT_TRACK_PERCENT_SKIP_ROUND
	inc 	<percentComplete 		; round up one percent 
FORMAT_TRACK_PERCENT_SKIP_ROUND

	lbsr 	PRINT_PERCENT_COMPLETE 	; show percentage progess of format to user 
	; send the write track command 
 IFNDEF skip_format
	lbsr 	WRITE_TRACK 
	lbcs 	ERROR_DISK_ACCESS
 ENDC 
 	ldb  	<numSides
 	cmpb  	#2 
 	blo  	FORMAT_TRACK_SKIP_SECOND_SIDE
	; modify track buffer for second side of disk while head is already there 
	ldb 	#1 		
	stb 	<side 
	lbsr 	CHANGE_SIDE
 IFNDEF skip_format
	; now write the second side of the current track 
	lbsr 	WRITE_TRACK 
	lbcs 	ERROR_DISK_ACCESS
 ENDC 
FORMAT_TRACK_SKIP_SECOND_SIDE
	; increment track counter to next value 
	ldb  	<track
	incb 
	cmpb 	tracksPerSide+1,U 
	bhs 	FINISHED
	stb 	<track
	; modify raw track buffer with the new track value 
	lbsr 	CHANGE_TRACK
	ldb  	<numSides
	cmpb  	#2 
	blo  	FORMAT_TRACK_NEXT  	; if single-sided only disk, skip reseting disk side
	; finally reset the side value back to first side (side 0) in track buffer 
	clr 	<side 		
	lbsr 	CHANGE_SIDE
	bra 	FORMAT_TRACK_NEXT

FINISHED
	ldu 	<uRegImage

	; check if user chose to verify disk 
	lda 	<verifyFlag
	beq 	VERIFY_SKIP
	lbsr 	VERIFY_FORMAT
	bcc 	VERIFY_SUCCESS 
	; something went wrong, report error and exit 
	leax 	strErrorVerify,PCR 
	lbsr 	PRINT_NULL_STR
	lbra 	EXIT 

VERIFY_SKIP
VERIFY_SUCCESS
	leax 	strFormatComplete,PCR 
	lbsr 	PRINT_NULL_STR 

CREATE_FAT12_FS
	; grab the system date/time 
	leax 	dateTimeData,U 
	os9 	F$Time 

	leax 	strVolumePrompt,PCR 
	lbsr  	PRINT_NULL_STR

	lda 	#STDIN
	leax 	volumeName,U 
	ldy 	#12 		; 11 chars + CR 
	os9 	I$ReadLn 

	leay 	paddedVolumeName,U 
	leax 	volumeName,U 
	; check first for empty/no name 
	lda 	,X 
	cmpa 	#C$CR 
	bne 	CREATE_FAT12_FS_VOL_NAME_CUSTOM
	leax 	strNoName,PCR 
	lbsr 	STRING_COPY_RAW
	bra 	CREATE_FAT12_FS_VOL_NAME_NONE
CREATE_FAT12_FS_VOL_NAME_CUSTOM
	; copy volume name with padding of spaces into null terminated string 
	ldb 	#11
CREATE_FAT12_FS_VOL_NAME_COPY
	lda 	,X+
	cmpa 	#C$CR 
	beq 	CREATE_FAT12_FS_VOL_NAME_ADD_PADDING
	lbsr 	CONVERT_UPPERCASE 
	sta 	,Y+
	decb 
	beq 	CREATE_FAT12_FS_VOL_NAME_DONE
	bra 	CREATE_FAT12_FS_VOL_NAME_COPY
CREATE_FAT12_FS_VOL_NAME_ADD_PADDING
	; fill remainder with spaces 
	lda 	#C$SPAC 
CREATE_FAT12_FS_VOL_NAME_PADDING_NEXT
	sta 	,Y+
	decb 
	bne 	CREATE_FAT12_FS_VOL_NAME_PADDING_NEXT
CREATE_FAT12_FS_VOL_NAME_DONE
	clr 	,Y 		; NULL terminator 
CREATE_FAT12_FS_VOL_NAME_NONE
	; before any sector writing, first seek to LSN #0 (track 0 sector 1) and read it.
	; OS9 WILL NOT BE ABLE TO WRITE TO SECTORS WITHOUT THIS FIRST 
	lda 	<drivePath
	ldx 	#0
	ldu 	#0
	os9 	I$Seek 
	lbcs 	ERROR_ACCESS_SECTORS
	ldu 	<uRegImage

	lda 	<drivePath
	leax 	rawTrackBuffer,U 
	ldy 	#512 
	os9 	I$Read 
	lbcs 	ERROR_ACCESS_SECTORS

	; now build the FAT12 file system into a buffer to write at the end 
	; first copy boot sector data into buffer.
	; this is part before volume name 
	ldb 	#bootSectorStartSz 		
	leay 	rawTrackBuffer,U 
	ldx 	<bootSectorPtr 
BOOT_SECTOR_PARAM_LOOP
	lda 	,X+
	sta 	,Y+
	decb 
	bne 	BOOT_SECTOR_PARAM_LOOP

	; generate a volume ID based off of the date/time similar to how DOS does 
	leax 	dateTimeData,U 
	lda 	D.Sec-D.Time,X 
	ldb 	D.Sec-D.Time,X  		; in MSDOS this would be 1/100 sec value 
	std 	volumeID+2,U 
	lda 	D.Month-D.Time,X
	ldb 	D.Day-D.Time,X 
	addd 	volumeID+2,U 
	; write result out little-endian for high word of volume ID 
	sta 	volumeID+3,U 
	stb 	volumeID+2,U 
	; now low word 
	clra 
	ldb 	D.Year-D.Time,X 
	addd 	#1900
	std 	volumeID,U 
	lda 	D.Hour-D.Time,X
	ldb 	D.Min-D.Time,X
	addd 	volumeID,U 
	; write result little endian for low word of volume ID 
	sta 	volumeID+1,U 
	stb 	volumeID,U 

	; copy into the sector buffer 
	ldd 	volumeID,U 
	std 	,Y++
	ldd 	volumeID+2,U 
	std 	,Y++ 

	; now copy padded volume name into boot sector 
	leax 	paddedVolumeName,U 
	lbsr 	STRING_COPY_RAW

	; now copy the rest of the boot sector/extended bios param block 
	leax 	bootSectorExtParam,PCR 
	ldb 	#bootSectorExtParamSz
BOOT_SECTOR_EXT_PARAM_LOOP
	lda 	,X+
	sta 	,Y+
	decb 
	bne 	BOOT_SECTOR_EXT_PARAM_LOOP

	; fill rest of 512 byte sector with 0's 
	ldd 	#bootSectorRemainder
	lsra 
	rorb  	; divide by 2 since loop does 2 bytes at a time 
	ldx 	#0
BOOT_SECTOR_ZERO_LOOP
	stx 	,Y++
	decb 
	bne 	BOOT_SECTOR_ZERO_LOOP

	; now build each empty FAT and copy them into the buffer 
	ldb 	fatTableCount,U 
	stb 	<fatCounter
BUILD_NEXT_FAT
	; assemble 1 complete FAT
	ldd  	<fatHeader 
	;ldd 	#$F9FF 			; 3 header bytes $F9FFFF
	std 	,Y++
	stb 	,Y+ 
	; calculate how many bytes a single FAT will be
	lda 	sectorsPerFAT,U 
	lsla 					; multiply by 2
	clrb 					; this trick is like multiplying sectorsPerFAT by 512
	subd 	#4 				; decrement count, 3 for the fat header, and 1 to make it even 
	lsra 
	rorb  	; divide by 2 
	; 0 out the rest 
FAT_ZERO_LOOP
	stx 	,Y++
	subd 	#1
	bne 	FAT_ZERO_LOOP
	clr 	,Y+ 			; zero the last odd byte 

	dec 	<fatCounter 
	bne 	BUILD_NEXT_FAT

	; finally zero out the root directory entries 
	sty 	<stackBlastStartPtr
	lda 	maxRootDirNum,U 
	ldb 	#32 					; 32 bytes in each directory entry 
	mul 
	addd 	<stackBlastStartPtr
	tfr 	D,U 
	std 	<rootDirEndPtr

	ldd 	#0
	ldx 	#0
	ldy 	#0 
ROOT_DIR_ZERO_LOOP
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D,X,Y 
	pshu 	D  					; 128 byte chunks 
	cmpu 	<stackBlastStartPtr
	bhi 	ROOT_DIR_ZERO_LOOP

	ldu 	<uRegImage
	; later DOS versions use first root dir entry for a volume label. copy the one 
	; set earlier here if user specified one, otherise skip/ignore this 
	lda 	volumeName,U 
	cmpa 	#C$CR 
	beq 	ROOT_DIR_SKIP_VOLUME_LABEL
	ldy 	<stackBlastStartPtr 	; this was pointer to start of root dir 
	leax 	paddedVolumeName,U 
	lbsr 	STRING_COPY_RAW
	; add the byte $28 which means the DIR entry is the drive's Volume Label 
	lda 	#$28 
	sta 	,Y 

	; now use the date/time to build a creation date to add to volume label entry
	ldy 	<stackBlastStartPtr 	; get ptr to start of root dir back 
	leax 	dateTimeData,U 
	lda 	D.Year-D.Time,X 
	suba 	#80  					; DOS years start at 1980 when value is 0 
	ldb 	D.Month-D.Time,X 
	lslb 
	lslb
	lslb
	lslb

	lslb 
	rola 
	orb 	D.Day-D.Time,X 
	stb 	$18,Y 
	sta 	$19,Y 

	lda 	D.Hour-D.Time,X 
	ldb 	D.Min-D.Time,X 
	lslb 
	lslb 

	lslb 
	rola 
	lslb 
	rola 
	lslb 
	rola 

	sta 	$17,Y 
	lsr 	D.Sec-D.Time,X 
	orb 	D.Sec-D.Time,X 
	stb 	$16,Y 
	; THAT WAS SOOO WEIRD BUT ITS DONE :P 

ROOT_DIR_SKIP_VOLUME_LABEL
	; seek to first sector on track 0
	lda 	<drivePath
	ldx 	#0
	ldu 	#0
	os9 	I$Seek 
	lbcs 	ERROR_ACCESS_SECTORS
	ldu 	<uRegImage

	lda 	<drivePath
	leax 	rawTrackBuffer,U 
WRITE_NEXT_SECTOR
	ldy 	#512 
	os9 	I$Write 
	lbcs 	ERROR_ACCESS_SECTORS
	leax 	512,X 
	cmpx 	<rootDirEndPtr
	blo 	WRITE_NEXT_SECTOR

	; calculate the allocation unit size 
	lda 	bytesPerSector+1,U  	; by using MSB only, its like automatically X 256 
	ldb 	sectsPerCluster,U
	mul 
	exg 	A,B 
	std 	allocSize,U 

	; calculate total disk size 
	clr 	totalBytes,U 
	clr 	totalBytes+3,U 
	lda 	totalSectors,U 		; LSB of total sectors 
	ldb 	bytesPerSector+1,U  	; MSB of bytes per sector since LSB is always $00
	mul 
	std 	totalBytes+1,U 	
	lda 	totalSectors+1,U 		; MSB of total sectors 
	ldb 	bytesPerSector+1,U  	; MSB of bytes per sector since LSB is always $00
	mul  
	addd 	totalBytes,U 
	std 	totalBytes,U 

	; calculate how many bytes root dir takes up 
	ldd 	#0
	std 	u32Value,U 
	lda 	maxRootDirNum,U 
	ldb 	#32 					; 32 bytes in each directory entry 
	mul 
	std 	u32Value+2,U 
	; calculate how many bytes per FAT 
	lda 	sectorsPerFAT,U 
	ldb 	fatTableCount,U 
	mul 
	lda 	bytesPerSector+1,U  	; use the MSB byte of bytes/sec to automatically X 256 
	mul 
	exg 	A,B 
	; add sum of bytes in all FATs to u32Value for later subtraction 
	addd 	u32Value+2,U 
	std 	u32Value+2,U 
	; add size of boot sector 
	lda 	bytesPerSector+1,U 
	ldb 	bytesPerSector,U 
	addd 	u32Value+2,U 
	std 	u32Value+2,U 

	; subtract it from total disk space possible 
	leax 	totalBytes,U 
	leay 	u32Value,U 
	lbsr 	SUBTRACT_32BIT

	leay 	strAsciiU32,U 
	leax 	totalBytes,U 
	lbsr 	CONVERT_BINARY32_DECIMAL

	; print the total disk space from the format 
	leay 	strBuffer,U 
	ldb 	#C$LF 
	stb 	,Y+
	ldb  	#13 		; start with 13 spaces before right-justifying text 
	subb 	u32strLength,U 
	; add space padding 
	lda 	#C$SPAC 
PRINT_RESULT_TOTAL_SPACE_PADDING
	sta 	,Y+
	decb 
	bne 	PRINT_RESULT_TOTAL_SPACE_PADDING
	leax 	strAsciiU32,U 
	lbsr 	STRING_COPY_RAW
	leax 	strDiskSpace,PCR 
	lbsr 	STRING_COPY_RAW
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	; now print the total availabe space 
	leay 	strBuffer,U 
	ldb  	#13 		; start with 13 spaces before right-justifying text 
	subb 	u32strLength,U 
	; add space padding 
	lda 	#C$SPAC 
PRINT_RESULT_AVAIL_SPACE_PADDING
	sta 	,Y+
	decb 
	bne 	PRINT_RESULT_AVAIL_SPACE_PADDING
	leax 	strAsciiU32,U 
	lbsr 	STRING_COPY_RAW
	leax 	strAvailSpace,PCR 
	lbsr 	STRING_COPY_RAW
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	; now print the bytes per allocation unit 
	ldd 	#0
	std 	u32Value,U 
	ldd 	allocSize,U 
	std 	u32Value+2,U 
	leay  	strAsciiU32,U 
	leax 	u32Value,U 
	lbsr 	CONVERT_BINARY32_DECIMAL

	leay 	strBuffer,U 
	ldb  	#13 		; start with 13 spaces before right-justifying text 
	subb 	u32strLength,U 
	; add space padding 
	lda 	#C$SPAC 
PRINT_RESULT_ALLOC_BYTES_PADDING
	sta 	,Y+
	decb 
	bne 	PRINT_RESULT_ALLOC_BYTES_PADDING
	leax 	strAsciiU32,U 
	lbsr 	STRING_COPY_RAW
	leax 	strBytesPerAlloc,PCR 
	lbsr 	STRING_COPY_RAW
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR	 	

	; calculate allocation units available by dividing
	; total AVAIABLE bytes by number of bytes per allocation unit.
	; we will cheat a bit and keep everything in multiples of 256 so we can 
	; leave out the last byte and do faster division 
	lda 	bytesPerSector+1,U 
	ldb 	sectsPerCluster,U 
	mul 
	std 	<u16Value 
	ldd 	totalBytes+1,U  		; use bytes 2 and 3 only out of 4 byte value 
	ldx 	#0 
PRINT_RESULT_ALLOC_AVAIL_DIV_NEXT
	subd 	<u16Value 
	bcs 	PRINT_RESULT_ALLOC_AVAIL_DIV_DONE
	leax 	1,X 
	bra 	PRINT_RESULT_ALLOC_AVAIL_DIV_NEXT

PRINT_RESULT_ALLOC_AVAIL_DIV_DONE
	stx 	u32Value+2,U 
	ldd 	#0
	std 	u32Value,U 
	leay 	strAsciiU32,U 
	leax 	u32Value,U 
	lbsr 	CONVERT_BINARY32_DECIMAL

	; print allocation units available 
	leay 	strBuffer,U 
	ldb  	#13 		; start with 13 spaces before right-justifying text 
	subb 	u32strLength,U 
	; add space padding 
	lda 	#C$SPAC 
PRINT_RESULT_ALLOC_AVAIL_PADDING
	sta 	,Y+
	decb 
	bne 	PRINT_RESULT_ALLOC_AVAIL_PADDING
	leax 	strAsciiU32,U 
	lbsr 	STRING_COPY_RAW
	leax 	strAllocUnitsAvail,PCR 
	lbsr 	STRING_COPY_RAW
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR	 	

	; finally print the volume serial number 
	leay 	strBuffer,U 
	leax 	strVolumeSerial,PCR 
	lbsr 	STRING_COPY_RAW
	lbsr 	CONVERT_VOLUME_ID
	ldd 	#$0D0A
	std 	,Y++
	clr 	,Y 
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	lbra 	EXIT 

ERROR_MISSING_DRIVE_PARAM
	leax  	strErrorMissingDrv,PCR 
	lbsr  	PRINT_NULL_STR
	bra   	EXIT_ONLY

ERROR_INVALID_PARAM
	leax 	strErrorParams,PCR 
	lbsr  	PRINT_NULL_STR
	bra 	EXIT_ONLY 

ERROR_INVALID_DISK_TYPE
	leax  	strErrorDiskType,PCR 
	lbsr  	PRINT_NULL_STR
	bra  	EXIT_ONLY

ERROR_ACCESS_SECTORS
	leax 	strErrorDiskSector,PCR 
	lbsr  	PRINT_NULL_STR
	bra 	EXIT 

ERROR_DISK_ACCESS
	leax 	strErrorDiskRaw,PCR 
	lbsr  	PRINT_NULL_STR

EXIT
	; restore previous PD settings for STDOUT 
	ldu 	<uRegImage 
	lda 	#STDIN
	leax 	stdinPD,U 
	ldb 	<prevEKO
	stb 	PD.EKO-PD.OPT,X 
	ldb 	#SS.Opt 
	os9 	I$SetStt 

	; close path to disk device 
	lda 	<drivePath
	os9 	I$Close 

EXIT_ONLY
	; return to OS9 shell 
	clrb 
	os9 	F$Exit 

; -----------------------------
; modify the raw track buffer to a different side value 
; Entry: side = value to change to (0 or 1)
; -----------------------------
CHANGE_SIDE
	pshs 	U,Y,X,D 

	ldu 	<uRegImage
	lda 	#9 			; 9 sectors 
	leax 	rawTrackBuffer+163,U 
	ldb 	<side 
CHANGE_SIDE_LOOP
	stb 	,X 
	;leax 	626,X 		; new SIDE value should be every 626 bytes from first 
	leax 	656,X 	
	deca 
	bne 	CHANGE_SIDE_LOOP

	puls 	D,X,Y,U,PC 

; -----------------------------
; modify the raw track buffer to a different track number 
; Entry: track = value to change to (0-79)
; -----------------------------
CHANGE_TRACK
	pshs 	U,Y,X,D 

	ldu 	<uRegImage
	lda 	#9 			; 9 sectors 
	leax 	rawTrackBuffer+162,U 
	ldb 	<track 
CHANGE_TRACK_LOOP
	stb 	,X 
	;leax 	626,X 		; new TRACK value should be every 626 bytes from first 
	leax 	656,X 		
	deca 
	bne 	CHANGE_TRACK_LOOP

	puls 	D,X,Y,U,PC 

; ---------------------------------
; write a raw track 
; ---------------------------------
WRITE_TRACK
	pshs 	U,Y,X,A

	ldu 	<uRegImage 
	leax 	rawTrackBuffer,U 
	clra 
	ldb 	<track 
	tfr 	D,U 
	ldb 	<side
	orb 	#%00000010 		; for double density 
	tfr 	D,Y 
	lda 	<drivePath
	ldb 	#SS.WTrk
	os9 	I$SetStt 

	puls 	A,X,Y,U,PC 	

; -------------------------------------------------
; verify the format 
; -------------------------------------------------
VERIFY_FORMAT
	pshs 	U,Y,X,D 

	ldu 	<uRegImage 

	; init some variables 
	clra 
	sta 	<percentComplete
	sta 	<verifyRemainder
	sta 	<verifyStepValue

	; update screen to tell user we are verifying now 
	leay 	strBuffer,U 
	leax 	strVerifying,PCR
	lbsr 	STRING_COPY_RAW

	ldb 	totalSectors,U 
	lda 	totalSectors+1,U
	std 	<verifySectorCounter
	lsra 
	rorb  
	lbsr 	CONVERT_BINARY16_DECIMAL
	lda 	#'K'
	sta 	,Y+
	ldd 	#$0D0A 
	std 	,Y++	
	clr 	,Y 
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	; caluclate percentage step counter increment 
	ldd 	<verifySectorCounter
VERIFY_FORMAT_DIVIDE
	subd 	#100 		; divide by 100 
	bcs 	VERIFY_FORMAT_DIV_DONE 
	inc 	<verifyStepValue
	bra 	VERIFY_FORMAT_DIVIDE
VERIFY_FORMAT_DIV_DONE
	addd 	#100 		; remainder back to use for fractional increments 
	stb 	<verifyRemainder 

	lda 	<drivePath
	ldx 	#0 
	ldu 	#0 
	os9 	I$Seek 
	ldu 	<uRegImage 
	bcs 	VERIFY_FORMAT_ERROR

	ldb 	<verifyStepValue
	stb 	<verifyCounter
VERIFY_FORMAT_NEXT
	lda 	<drivePath 
	leax 	rawTrackBuffer,U 
	ldy 	#512 
	os9 	I$Read 
	bcs 	VERIFY_FORMAT_ERROR 

	dec 	<verifyCounter 
	bne 	VERIFY_FORMAT_SKIP_PERCENT_INC
	inc 	<percentComplete
	lda 	<verifyStepValue
	ldb 	<verifyRemCounter 
	addb 	<verifyRemainder
	cmpb 	#100 
	blo 	VERIFY_FORMAT_SAVE_COUNTERS
	inca 						; an extra number to the normal step count for fractions
	subb 	#100
VERIFY_FORMAT_SAVE_COUNTERS
	stb 	<verifyRemCounter
	sta 	<verifyCounter

	; now print percent progress to screen 
	lbsr 	PRINT_PERCENT_COMPLETE

VERIFY_FORMAT_SKIP_PERCENT_INC
	ldd 	<verifySectorCounter
	subd 	#1
	std 	<verifySectorCounter
	bne 	VERIFY_FORMAT_NEXT 
	; ANNND COMPLETED SUCCESSFULLY! 

	andcc 	#$FE 
	puls 	D,X,Y,U,PC 

VERIFY_FORMAT_ERROR
	; print current POS 
	lda 	<drivePath
	ldb 	#SS.Pos 
	os9 	I$GetStt 
	stx 	<u16Value 
	lbsr 	PRINT_WORD_HEX
	stu 	<u16Value
	lbsr 	PRINT_WORD_HEX
	ldu 	<uRegImage 
	orcc 	#1 
	puls 	D,X,Y,U,PC 

; -------------------------------------------------
; calculate a percentage based on number of tracks 
; -------------------------------------------------
PRINT_PERCENT_COMPLETE
	pshs 	U,Y,X,D 

	ldu 	<uRegImage

	ldb 	<percentComplete
	leay 	temp,U 
	lbsr 	CONVERT_BYTE_DEC
	; start building the output string to display progress 
	leay 	strBuffer,U 
	ldd 	#$030D 		; erase line, carriage return 
	std 	,Y++
	; pad the percent value with spaces like MSDOS does 
	ldb 	#3
	subb 	<asciiByteDecLen
	beq 	PRINT_PERCENT_COMPLETE_NO_PAD_NEEDED
	lda 	#C$SPAC 
PRINT_PERCENT_COMPLETE_PAD_LOOP
	sta 	,Y+
	decb 
	bne 	PRINT_PERCENT_COMPLETE_PAD_LOOP
PRINT_PERCENT_COMPLETE_NO_PAD_NEEDED
	; append the ascii result of percentage 
	leax 	temp,U 
	lbsr 	STRING_COPY_RAW
	; append the description for user and print it to stdout 
	leax 	strPercentCompleted,PCR 
	lbsr 	STRING_COPY_RAW
	leax 	strBuffer,U 
	lbsr 	PRINT_NULL_STR

	puls 	D,X,Y,U,PC 

; ------------------------------------------------------
; setup various disk variables for total sectors, number
; of sides, etc
; Entry: Y = pointer to start of boot sector constants
; 	 		 for corresponding disk type 
; ------------------------------------------------------
POPULATE_DISK_SETTINGS
	pshs  	D  

	sty  	<bootSectorPtr
	ldd  	11,Y 
	std  	<bytesPerSector
	lda   	13,Y 
	sta  	<sectsPerCluster
	lda  	16,Y 
	sta  	<fatTableCount
	ldd  	17,Y 
	std  	<maxRootDirNum
	ldd  	19,Y 
	std  	<totalSectors
	lda  	21,Y   			; first byte of fat header should mirror the media descriptor byte
	ldb  	#$FF 
	std   	<fatHeader 
	ldd  	22,Y 
	std  	<sectorsPerFAT
	ldd  	26,Y 
	std  	<numSides
	; setup OS9 drive parameters in the device descriptor
	ldd  	-7,Y 
	std  	<os9driveTYP  	; TYP in A and DNS in B 
	ldd  	-5,Y  			; CYL for tracks per side
	std  	<tracksPerSide
	ldd  	-3,Y  			; SCT/T0S 
	std   	<os9driveSCT
	lda  	-1,Y  			; ILV 
	sta  	<os9driveILV

	puls  	D,PC 

; ------------------------------------------------------
; generate an ascii volume ID in hex 
; Entry: volumeID = 32 bit volume ID number 
; 		 Y = pointer to place to store ascii hex result 
; Exit: Y = points to final terminating NULL at end 
; ------------------------------------------------------
CONVERT_VOLUME_ID
	pshs 	X,D

	leax 	asciiHexList,PCR
	; first do Most Significan Word 
	lda 	volumeID+3,U
	lsra 
	lsra
	lsra
	lsra
	lda 	A,X
	sta 	,Y+
	lda 	volumeID+3,U 
	anda 	#$0F 
	lda 	A,X 
	sta 	,Y+
	lda 	volumeID+2,U
	lsra 
	lsra
	lsra
	lsra
	lda 	A,X
	sta 	,Y+
	lda 	volumeID+2,U
	anda 	#$0F 
	lda 	A,X 
	sta 	,Y+
	lda 	#'-'
	sta 	,Y+

	; now the Least Significant Word 
	lda 	volumeID+1,U 
	lsra 
	lsra
	lsra
	lsra
	lda 	A,X
	sta 	,Y+
	lda 	volumeID+1,U  
	anda 	#$0F 
	lda 	A,X 
	sta 	,Y+
	lda 	volumeID,U
	lsra 
	lsra
	lsra
	lsra
	lda 	A,X
	sta 	,Y+
	lda 	volumeID,U
	anda 	#$0F 
	lda 	A,X 
	clrb 
	std 	,Y+

	puls 	D,X,PC

; -----------------------------
; convert 8-bit binary value to ascii decimal
; Entry: B = value to be printed in decimal ASCII 
;        Y = destination to write result 
; --------------------------------
CONVERT_BYTE_DEC
      pshs  X,D 

      clr 	<asciiByteDecLen
      ldx   #$0000      ; use X as flag to tell if we need to ignore leading zeros
      clra 
CONVERT_BYTE_DEC_INC_100S
      subb  #100 
      blo   CONVERT_BYTE_DEC_JUMP_10S
      inca 
      leax  1,X
      bra   CONVERT_BYTE_DEC_INC_100S
CONVERT_BYTE_DEC_JUMP_10S
      cmpx  #$0000
      beq   CONVERT_BYTE_DEC_SKIP_100S
      adda  #$30  ; the magic ASCII number 
      sta   ,Y+
      inc 	<asciiByteDecLen
CONVERT_BYTE_DEC_SKIP_100S
      clra        ; reset counter
      addb  #100
CONVERT_BYTE_DEC_INC_10S
      subb  #10
      blo   CONVERT_BYTE_DEC_JUMP_1S 
      inca 
      leax  1,X   
      bra   CONVERT_BYTE_DEC_INC_10S
CONVERT_BYTE_DEC_JUMP_1S
      cmpx  #$0000
      beq   CONVERT_BYTE_DEC_SKIP_10S
      adda  #$30
      sta   ,Y+
      inc 	<asciiByteDecLen
CONVERT_BYTE_DEC_SKIP_10S
      addb  #$3A  ; $30 ASCII '0' + 10 from previous subtraction 
      stb   ,Y+
      inc 	<asciiByteDecLen

      clr   ,Y          ; NULL temrinator 

      puls  D,X,PC

; -----------------------------------
; convert 16bit value to decimal ascii 
; Entry: Y = pointer to where to copy resulting ascii string 
; 	D = value to be converted 
; Exit: Y = pointer to AFTER the null-terminator of output 
; -----------------------------------
CONVERT_BINARY16_DECIMAL
	pshs 	X,D 

	leax 	temp,U 
	ldd 	#$3030 	; ascii 0
	std 	,X
	std 	2,X 

	ldd 	,S 
CONVERT_BINARY16_DECIMAL_NEXT_10000
	subd 	#10000
	bcs 	CONVERT_BINARY16_DECIMAL_DO_1000
	inc 	,X 
	bra 	CONVERT_BINARY16_DECIMAL_NEXT_10000
CONVERT_BINARY16_DECIMAL_DO_1000
	addd 	#10000
CONVERT_BINARY16_DECIMAL_NEXT_1000
	subd 	#1000
	bcs 	CONVERT_BINARY16_DECIMAL_DO_100
	inc 	1,X 
	bra 	CONVERT_BINARY16_DECIMAL_NEXT_1000
CONVERT_BINARY16_DECIMAL_DO_100
	addd 	#1000
CONVERT_BINARY16_DECIMAL_NEXT_100
	subd 	#100
	bcs 	CONVERT_BINARY16_DECIMAL_DO_10
	inc 	2,X 
	bra 	CONVERT_BINARY16_DECIMAL_NEXT_100
CONVERT_BINARY16_DECIMAL_DO_10
	addd 	#100
CONVERT_BINARY16_DECIMAL_NEXT_10
	subd 	#10
	bcs 	CONVERT_BINARY16_DECIMAL_DO_1
	inc 	3,X 
	bra 	CONVERT_BINARY16_DECIMAL_NEXT_10
CONVERT_BINARY16_DECIMAL_DO_1
	addd 	#10
	addb 	#$30
	stb 	4,X 
	clr 	5,X 

CONVERT_BINARY16_DECIMAL_SKIP_LEADING_ZEROES
	tst 	,X 
	beq 	CONVERT_BINARY16_DECIMAL_SKIP_LEADING_ZEROES_SINGLE
	ldb 	,X+
	cmpb 	#$30
	beq 	CONVERT_BINARY16_DECIMAL_SKIP_LEADING_ZEROES
CONVERT_BINARY16_DECIMAL_SKIP_LEADING_ZEROES_SINGLE
	leax 	-1,X 
	clrb 
CONVERT_BINARY16_DECIMAL_COPY
	incb
	lda 	,X+
	sta 	,Y+
	bne 	CONVERT_BINARY16_DECIMAL_COPY
	decb 
	stb 	u16strLength,U 

	leay 	-1,Y 		; undo last auto-increment 

	puls 	D,X,PC 

; -------------------------------------
; convert 32 bit value to comma delimited decimal number 
; Entry: X = pointer to 32 bit value 
; 	Y = pointer to destination to write ASCII result 
; Exit:  B = length of resulting ascii string (not including null terminator)
; -------------------------------------
CONVERT_BINARY32_DECIMAL
	pshs 	Y,X,D 

	ldd 	,X 
	std 	u32Value,U 
	ldd 	2,X 
	std 	u32Value+2,U 

	ldd 	#"0,"
	std 	<temp 
	ldd 	#"00"
	std 	<temp+2
	ldd 	#"0,"
	std 	<temp+4
	ldd 	#"00"
	std 	<temp+6
	ldd 	#"0,"
	std 	<temp+8
	ldd 	#"00"
	std 	<temp+10
	ldd 	#$3000
	std 	<temp+12

	leax 	u32Value,U 
	leay 	bin32dec1B,PCR 
CONVERT_BINARY32_DECIMAL_NEXT_1B
	lbsr 	SUBTRACT_32BIT
	bcs 	CONVERT_BINARY32_DECIMAL_DO_100M
	inc 	<temp
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_1B
CONVERT_BINARY32_DECIMAL_DO_100M
	lbsr 	ADD_32BIT
	leay 	bin32dec100M,PCR 
CONVERT_BINARY32_DECIMAL_NEXT_100M
	lbsr 	SUBTRACT_32BIT
	bcs 	CONVERT_BINARY32_DECIMAL_DO_10M
	inc 	<temp+2
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_100M
CONVERT_BINARY32_DECIMAL_DO_10M
	lbsr 	ADD_32BIT
	leay 	bin32dec10M,PCR 
CONVERT_BINARY32_DECIMAL_NEXT_10M
	lbsr 	SUBTRACT_32BIT
	bcs 	CONVERT_BINARY32_DECIMAL_DO_1M
	inc 	<temp+3
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_10M
CONVERT_BINARY32_DECIMAL_DO_1M
	lbsr 	ADD_32BIT
	leay 	bin32dec1M,PCR 
CONVERT_BINARY32_DECIMAL_NEXT_1M
	lbsr 	SUBTRACT_32BIT
	bcs 	CONVERT_BINARY32_DECIMAL_DO_100K
	inc 	<temp+4
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_1M
CONVERT_BINARY32_DECIMAL_DO_100K
	lbsr 	ADD_32BIT
	leay 	bin32dec100K,PCR 
CONVERT_BINARY32_DECIMAL_NEXT_100K
	lbsr 	SUBTRACT_32BIT
	bcs 	CONVERT_BINARY32_DECIMAL_DO_10K
	inc 	<temp+6
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_100K
CONVERT_BINARY32_DECIMAL_DO_10K
	lbsr 	ADD_32BIT
	leay 	bin32dec10K,PCR 
CONVERT_BINARY32_DECIMAL_NEXT_10K
	lbsr 	SUBTRACT_32BIT
	bcs 	CONVERT_BINARY32_DECIMAL_DO_1K
	inc 	<temp+7
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_10K
CONVERT_BINARY32_DECIMAL_DO_1K
	lbsr 	ADD_32BIT
	ldd 	u32Value+2,U 
CONVERT_BINARY32_DECIMAL_NEXT_1K
	subd 	#1000
	bcs 	CONVERT_BINARY32_DECIMAL_DO_100
	inc 	<temp+8
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_1K
CONVERT_BINARY32_DECIMAL_DO_100
	addd 	#1000
CONVERT_BINARY32_DECIMAL_NEXT_100
	subd 	#100
	bcs 	CONVERT_BINARY32_DECIMAL_DO_10
	inc 	<temp+10
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_100
CONVERT_BINARY32_DECIMAL_DO_10
	addd 	#100
CONVERT_BINARY32_DECIMAL_NEXT_10
	subd 	#10
	bcs 	CONVERT_BINARY32_DECIMAL_DO_1
	inc 	<temp+11
	bra 	CONVERT_BINARY32_DECIMAL_NEXT_10
CONVERT_BINARY32_DECIMAL_DO_1
	addd 	#10
	addb 	#$30
	stb 	<temp+12

	; move pointer to eliminate leading zeroes 
	ldy 	4,S 		; get Y back from the stack 
	leax 	temp,U 
CONVERT_BINARY32_DECIMAL_SKIP_COMMA
	lda 	,X+
	beq 	CONVERT_BINARY32_DECIMAL_ZERO_RESULT
	cmpa 	#','
	beq 	CONVERT_BINARY32_DECIMAL_SKIP_COMMA
	cmpa 	#$30
	bne 	CONVERT_BINARY32_DECIMAL_FOUND
	bra 	CONVERT_BINARY32_DECIMAL_SKIP_COMMA
CONVERT_BINARY32_DECIMAL_ZERO_RESULT
	leax 	-1,X 
CONVERT_BINARY32_DECIMAL_FOUND
	leax 	-1,X
	clrb 
CONVERT_BINARY32_DECIMAL_COPY
	incb 
	lda 	,X+
	sta 	,Y+
	bne 	CONVERT_BINARY32_DECIMAL_COPY
	decb 
	stb 	u32strLength,U 

	puls 	D,X,Y,PC 

;---------------------------------
; convert to uppercase 
; Entry: A = character to be converted 
; Exit: A = converted character 
; --------------------------------
CONVERT_UPPERCASE
      ; check and/or convert lowercase to uppercase
      cmpa  #$61        ; $61 is "a"
      blo   CONVERT_UPPERCASE_NO_CONVERSION
      cmpa  #$7A  ; $7A is "z"
      bhi   CONVERT_UPPERCASE_NO_CONVERSION
      suba  #$20  ; convert from lowercase to uppercase 
CONVERT_UPPERCASE_NO_CONVERSION
      rts 

; ----------------------------------------------------------------------------------------
; 32 bit subtraction 
; Entry: X = pointer to 32 bit source number we are subtracting from, and store result in 
; 		 Y = pointer to 32 bit number to subtract from value pointed to by X 
; ----------------------------------------------------------------------------------------
SUBTRACT_32BIT
      pshs  D
      ldd   2,X 
      subd  2,Y 
      std   2,X 
      ldd   ,X 
      sbcb  1,Y 
      sbca  ,Y 
      std   ,X 

      ; carry should be set properly now from subtract, now make sure zero flag works too
      ;ldd   ,X   ; not needed cuz previous instruction was STD ,X which already sets the Z and N flags 
      bne   SUBTRACT_32BIT_NOT_ZERO
      ldd   2,X
      andcc #%11110111 
SUBTRACT_32BIT_NOT_ZERO
      puls  D,PC 

 ;----------------------------------
 ; 32 bit addition 
 ; ---------------------------------
ADD_32BIT
      pshs  D 
      ldd   2,X 
      addd  2,Y 
      std   2,X 
      ldd   ,X 
      adcb  1,Y 
      adca  ,Y 
      std   ,X 

      bne   ADD_32BIT_NOT_ZERO
      ldd   2,X 
      andcc #%11110111
ADD_32BIT_NOT_ZERO
      puls  D,PC 

; ------------------------------------------------
; search ascii string for first space, NULL, or CR 
; Entry: X = pointer to string to search 
; Exit: on success, carry clear, 
; 	 B = number of characters before terminator, X = pointing to terminating char 
; 	 on fail, carry set
; ------------------------------------------------
FIND_NEXT_SPACE_NULL_CR
	pshs 	A 

	clrb 
FIND_NEXT_SPACE_NULL_CR_NEXT
	lda 	,X+
	beq 	FIND_NEXT_SPACE_NULL_CR_END
	cmpa 	#$20
	beq 	FIND_NEXT_SPACE_NULL_CR_END
	cmpa 	#C$CR 
	beq 	FIND_NEXT_SPACE_NULL_CR_END
	incb 
	bne 	FIND_NEXT_SPACE_NULL_CR_NEXT
	orcc 	#1 	; set carry for overflow error
	puls 	A,PC 

FIND_NEXT_SPACE_NULL_CR_END
	leax 	-1,X 
	andcc 	#$FE 	; carry clear for success 
	puls 	A,PC 

; --------------------------------------------------------------------------------
; copy a raw string, including control codes, etc until NULL 
; Entry: X = source pointer, Y = Destination Pointer 
; Exit: carry set = fail, carry clear success, Y = pointer to final NULL in dest 
; --------------------------------------------------------------------------------
STRING_COPY_RAW
	pshs 	X,A 
	clrb 
STRING_COPY_RAW_NEXT
	lda 	,X+
	sta 	,Y+
	beq 	STRING_COPY_RAW_DONE
	decb 
	bne 	STRING_COPY_RAW_NEXT
	coma 	; set carry for error 
	puls 	A,X,PC 

STRING_COPY_RAW_DONE
	leay 	-1,Y 		; undo auto-increment 
	; carry already cleared from STA of NULL 
	puls 	A,X,PC 

; ----------------------------------------------
; copy a parameter word delimitted by a NULL, 
; CR, or SPACE.
; Exit: X = points to ending/terminating character
; 	B = number of bytes written to Y pointer 
; 	Y = pointing to the NULL at end of destination 
; ----------------------------------------------
PARAM_COPY
	pshs 	A 
	clrb 
PARAM_COPY_NEXT_CHAR
	lda 	,X+
	beq 	PARAM_COPY_DONE
	cmpa 	#C$CR 
	beq 	PARAM_COPY_DONE
	cmpa 	#C$SPAC 
	beq 	PARAM_COPY_DONE
	sta 	,Y+
	incb 
	bne 	PARAM_COPY_NEXT_CHAR
	orcc 	#1
	puls 	A,PC 
PARAM_COPY_DONE
	leax 	-1,X 
	clr 	,Y 	; mark NULL in destination 
	andcc 	#$FE
	puls 	A,PC 

; ---------------------------------------
; scan for another flag parameter 
; Entry: X = pointing to string to search 
; Exit: on success, carry clear, X is pointing to the flag character returned 
;       on fail, carry set, X is pointer to first non-space char 
; ---------------------------------------
SEARCH_PARAMETER_FLAG
      pshs  A

      lbsr  SEARCH_NEXT_NONSPACE
      lda   ,X+
      cmpa  #'-'
      bne   SEARCH_PARAMETER_FLAG_NONE
      andcc #$FE        ; carry clear success
      puls  A,PC              

SEARCH_PARAMETER_FLAG_NONE
      leax  -1,X        ; undo auto-increment 
      orcc  #1          ; not found 
      puls  A,PC 

; -------------------------------------------------------
; scan for the next non-space character 
; Entry: X = pointing to area of string to search through 
; Exit: on success, carry clear, X points to the first non-space character 
;       on fail, carry set, X is restored to original value 
; -------------------------------------------------------
SEARCH_NEXT_NONSPACE
      pshs  X,D 

      clrb 
SEARCH_NEXT_NONSPACE_NEXT
      lda   ,X+
      cmpa  #$20
      bne   SEARCH_NEXT_NONSPACE_DONE
      decb 
      bne   SEARCH_NEXT_NONSPACE_NEXT
      ; never found a non-space character within 256 bytes 
      orcc  #1
      puls  D,X,PC            ; restore everything and return 

SEARCH_NEXT_NONSPACE_DONE      
      leax  -1,X              ; reverse the auto-increment
      puls  D 
      leas  2,S               ; skip X on the stack 
      andcc #$FE              ; carry clear on success 
      rts                     ; return with X pointing to non-space character 

; ---------------------------------------------
; print a null-terminated string to STDOUT 
; Entry: X = pointer to string to print 
; ---------------------------------------------
PRINT_NULL_STR
	pshs 	Y,X,D

	ldy  	#$FFFF
PRINT_NULL_STR_NEXT
	leay  	1,Y 
	lda 	,X+
	bne 	PRINT_NULL_STR_NEXT
	ldx  	2,S 
	lda 	#STDOUT
	os9 	I$Write

	puls 	D,X,Y,PC 


*****************************************************
; debug routines to remove later 

; --------------------------------
; print hex byte value 
; --------------------------------
PRINT_BYTE_HEX
      pshs  U,Y,X,D

      ldu   <uRegImage

      leax  asciiHexList,PCR
      leay 	strHexWord,U 
      lda   #'$'
      sta   ,Y  
   
      lda   <u8Value
      lsra 
      lsra
      lsra
      lsra
      lda   A,X
      sta   1,Y              ; store first digit

      lda   <u8Value 
      anda  #$0F
      lda   A,X 
      sta   2,Y               ; store second digit

      lda 	#C$CR 
      sta 	3,Y 

      lda   #STDOUT
      ldy   #200
      leax  strHexWord,U 
      os9   I$WritLn

      puls  D,X,Y,U,PC 

; -----------------------------
; Print 16 bit value in hex store in u16Value
; Entry: u16Value  = contains 16 bit value to be printed
; ------------------------------
PRINT_WORD_HEX
	pshs 	U,Y,X,D

	ldu 	<uRegImage 
	leax 	asciiHexList,PCR
	leay 	strHexWord,U 
	lda 	#'$'
	sta 	,Y

	lda 	<u16Value 
	lsra 
	lsra
	lsra
	lsra
	lda 	A,X
	sta 	1,Y
	lda 	<u16Value 
	anda 	#$0F 
	lda 	A,X 
	sta 	2,Y
	lda 	<u16Value+1
	lsra 
	lsra
	lsra
	lsra
	lda 	A,X
	sta 	3,Y
	lda 	<u16Value+1
	anda 	#$0F 
	lda 	A,X 
	ldb 	#C$SPAC
	std 	4,Y 

	lda 	#STDOUT 
	leax 	strHexWord,U 
	ldy 	#6
	os9 	I$Write

	puls 	D,X,Y,U,PC

DELAY
	pshs 	X 

	ldx 	#0 
DELAY_LOOP
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	exg 	D,D 
	leax 	1,X 
	bne 	DELAY_LOOP

	puls 	X,PC 


*************************************************************************************
	EMOD 
MODULE_SIZE 	; put this at the end so it can be used for module size 

