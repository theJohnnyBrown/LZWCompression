TITLE LZWJohnny

;Johnny Brown

INCLUDE Irvine32.inc
.data
greet BYTE "LZWJohnny 1.0",0ah,0dh,"LZW file compression, implemented by Johnny Brown",0ah,0dh,"guaranteed to work on text files up to 185 KB",0ah,0dh,"enter 1 to compress a file, or any other key to decompress",0ah,0dh,0
inPrompt BYTE "Enter the input path and filename: ",0
outPrompt BYTE "Enter the output path and filename: ",0
error BYTE "A file error occurred (maybe)",0ah,0dh,0
doOver BYTE "press space to return to the main menu to compress/decompress another file, or any other key to exit",0ah,0dh,0
choice BYTE ?
filename BYTE 300 dup(?)
newfilename BYTE "newOutput.jb",0
hand DWORD ?
dictIndex DWORD 0
inPtr DWORD 0
outPtr DWORD 0
filesize DWORD ?
dictKeys DWORD 131070 DUP(?)
dictVals BYTE 150000 DUP(?)
inFile BYTE 200000 DUP(?)
outFile byte 200000 DUP(?)

.code ;----------------------------------------------------------------------------------------------------

main PROC

;THE "MAIN MENU":
menu:
MOV edx,OFFSET greet
call writeString
call readChar
MOV choice,al

;GET THE INPUT FILENAME:
MOV edx,OFFSET inPrompt
call WriteString
MOV edx,OFFSET filename
MOV ecx,300
call readString
call crlf

;GET THE OUTPUT FILENAME
MOV edx,OFFSET outPrompt
call WriteString
MOV edx,OFFSET newFileName
MOV ecx,300
call readString
call crlf

;OPEN AND DISPLAY THE FILE:
MOV edx,OFFSET filename
call openAndRead
mov edx,OFFSET inFile
;call WriteString
cmp eax,INVALID_HANDLE_VALUE
JE fileError

;SET FILE POINTERS
  MOV eax,OFFSET inFile
  MOV inPtr,eax
  MOV eax,OFFSET outFile
  MOV outPtr,eax
    
;FILL THE DICTIONARY FOR 0-256
  MOV dictIndex,0
  MOV ecx,256
  MOV eax,0
  MOV edx,dictIndex
  MOV ebx,OFFSET dictVals
  fillDict:
    MOV dictKeys[edx],ebx
    MOV [ebx],al
    INC eax
    iNC ebx
    ADD edx,4
    MOV dictKeys[edx],1
    ADD edx,4
    
  LOOP fillDict
  SUB edx,8
  MOV dictIndex,edx

MOV al,'1'
CMP al,choice
JE doCompression
call decompress
jmp aazz
doCompression:
call compress
aazz:
MOV edx,OFFSET doOver
call writeString
call readChar
MOV choice,al
MOV al,' '
CMP al,choice
JE menu

exit
main ENDP
;----------------------------------------------------------------------------------------------------------------------------
decompress PROC
;DO THE DECOMPRESSION
mainLoop:
MOV esi,inPtr
xor eax,eax
MOV ax,[esi]
push eax

call getTheSequence

CMP eax,-1
JE myelse
;if ^eax!=-1
  push eax; see \/
  push ecx;for later, the call will clean the last two from the stack
  push eax
  push ecx
  call WriteToOutput;write the sequence we know to output  
  add inPtr,2
   MOV esi,inPtr
   xor eax,eax
   MOV ax,[esi]
   
   push eax
   
  call firstChar;if we don't know the next sequence, FIRSTCHAR will return the first char of the previous sequence    
  ;firstChar is in bl
  pop ecx;length of known entry
  pop eax;offset of known entry
  inc ecx;length of new entry
  PUSH ebx;will be added to the end of the new dict entry
  push eax
  push ecx
  call ADDTODICT
   MOV esi,OFFSET infile
   ADD esi,filesize
   CMP inPtr,esi
   JGE decoded
   JMP mainLoop
myelse:

  MOV esi,inPtr
  xor eax,eax
  MOV ax,[esi]
  SUB ax,2
  push eax
  call firstChar
  PUSH ebx
  push eax
  call getTheSequence
  push eax
  push ecx
  call addToDict
  JMP mainLoop


decoded:
;WRITE OUTPUT TO THE FILE
MOV edx,OFFSET newfilename
call writeContents
cmp eax,INVALID_HANDLE_VALUE
JE fileError

jmp decdone
fileError:
mov edx,OFFSET error
call dumpregs
;call WriteString
decdone:
mov eax,hand
call CloseFile

decompress ENDP
;-----------------------------------------------------------------------------------------------------------------------------
compress PROC


lzwLoop:
;until the end of the file: call step
;step writes one sequence to the dictionary and one (dword)sequence to the output
;increases inPtr, dictIndex
MOV eax,inptr
SUB eax,OFFSET infile
CMP eax,filesize
JGE encoded

call step
jmp lzwloop
encoded:

;WRITE OUTPUT TO THE FILE
MOV edx,OFFSET newfilename
call writeContents
cmp eax,INVALID_HANDLE_VALUE
JE loc_fileError

jmp done
loc_fileError:
mov edx,OFFSET error

call WriteString

done:

mov eax,hand
call CloseFile
compress ENDP

;-----------------------------------------------------------------------------------
step PROC
  PUSH ebp
  MOV ebp,esp
  
  XOR edx,edx
  pushLoop:
    PUSH inPtr
    INC edx
    PUSH EDX
    call getIndexOf  
    CMP eax,-1
  JNE pushLoop
  PUSH edx

  
;  ;ADD THE UNKNOWN STRING TO THE DICTIONARY:
;  MOV ([dict[dictIndex]]+[dict[dictIndex+4]]),(the bytes b/w [inptr] and infile[edx])
;  ADD dictIndex,8
;  MOV dictKeys[dictindex],([dictKeys[dictIndex]]+[dictKeys[dictIndex+4]])
;  MOV dict[dictindex+1],ebp-esp


  MOV esi,dictIndex
  ADD esi,OFFSET dictKeys ;esi points to addr of last dictVal added
  MOV edi,[esi]
  ADD edi,[esi+4];edi contains addr of next dictVal
  MOV [esi+8],edi
  ADD dictIndex,8
  
  MOV eax,inPtr
  MOV ecx,[esp] ; length of new string
  MOV esi,dictIndex
  add esi,OFFSET dictKeys
  mov [esi+4],ecx
  addString:    
    MOV bl,[eax]
    MOV [edi],bl
    INC eax
   
    INC edi
  LOOP addString  

;  
;  ;PRINT A CODE TO outFile (outfile will be DWORD. collapse will happen at the end)
  POP ecx
  dec ecx
  PUSH inPtr
  PUSH ecx
  ADD inPtr,ecx
  XOR edx,edx;because div instruction uses it
  call getIndexOf
  MOV ebx,8
  DIV ebx
  MOV esi,outPtr
  MOV [esi],ax
  ADD outPtr,2
  


  POP ebp
  ret
step ENDP
;-----------------------------------------------------------------------------------
byteArraysEqual PROC; push OFFSET array1, SIZEOF array1, OFFSET array2, SIZEOF array2 
                    ;before call, all DWORDs. Returns eax = 0 if arrays are equal
  PUSH ebp
  MOV ebp,esp

  MOV esi,[ebp + 20]; OFFSET array1
  MOV edi,[ebp + 12]; OFFSET array2
  MOV eax,[ebp + 16]; SIZEOF array1
  CMP eax,[ebp + 8]; check for same size
  JE sizeEqual
  jmp notEqual
  sizeEqual:
  
  MOV ecx,eax
  ADD ecx,esi
  L1:
    MOV al,[esi]
    CMP al,[edi]
    JNE notEqual
    INC esi
    INC edi
    CMP esi,ecx
  JB L1
  XOR eax,eax
  notEqual:

  POP ebp
  ret 16
byteArraysEqual ENDP
;-----------------------------------------------------------------------------------
openAndRead PROC ;Filename in EDX, Buffer in variable "inFile".
  ;OPEN THE FILE
  call OpenInputFile
  cmp eax,INVALID_HANDLE_VALUE
  je noFile
  mov hand,eax

  ;READ THE CONTENTS TO MEMORY
  MOV edx,OFFSET inFile
  MOV ecx,SIZEOF inFile
  MOV eax,hand
  call ReadFromFile
  MOV filesize,eax

  jmp readEnd
  noFile:
  call fileError
  readEnd:
  ret
openAndRead ENDP
;-----------------------------------------------------------------------------------
writeContents PROC ;writes variable "outFile" to a new file, filename in edx
  mov esi,OFFSET outFile
  mov ecx,10
  mov ebx,4
  call CreateOutputFile
  cmp eax,INVALID_HANDLE_VALUE
  JE cantCreate

  MOV edx,OFFSET outFile
  MOV ecx,outptr
  SUB ecx,edx
  call writetofile

  jmp writeEnd
  cantCreate:
  ;call fileError
  writeEnd:
  ret
writeContents ENDP
;-----------------------------------------------------------------------------------
getIndexOf PROC; addr of value to get at [ebp+12], length of value to get (in bytes)
               ; at [ebp+8]. returns index of the given value's key, 
               ;or -1 if not found, in eax

  PUSH ebp
  MOV ebp,esp
  
   
    
  MOV ecx,dictIndex

  findLoop:
    PUSH ecx
    PUSH dictKeys[ecx]
    PUSH dictKeys[ecx+4]
    PUSH [ebp+12]
    PUSH [ebp+8]
    
    call byteArraysEqual
    POP ecx
    CMP eax,0
    JE found
    SUB ecx,8
    CMP ecx,0
  JGE findloop
  XOR ecx,ecx
  DEC ecx
  
  found:
  MOV eax,ecx

  POP ebp
  ret 8
getIndexOf ENDP
;-----------------------------------------------------------------------------------
fileError PROC
  MOV edx,OFFSET error
  call WriteString
  mov eax,INVALID_HANDLE_VALUE
  ret
fileError ENDP
;--------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------
addToDict PROC ;newChar DWORD, oldOffset DWORD, newLength DWORD
push ebp
mov ebp,esp
MOV esi,[ebp+12]
MOV edx,dictIndex
mov edi,dictKeys[edx];offset of last dictval
ADD edx,4
ADD edi,dictKeys[edx];length of last dictval. edi points to new dictval
add edx,4
mov dictIndex,edx
mov dictkeys[edx],edi
MOV ecx,[ebp+8]
;call dumpregs
mov dictkeys[edx+4],ecx
DEC ecx

addEntry:
  MOV al,[esi]
  MOV [edi],al
  inc esi
  inc edi
LOOP addentry
mov al,[ebp+16]
mov [edi],al


pop ebp
ret 12
addToDict ENDP
;--------------------------------------------------------------------------------------------------------------
firstChar PROC;takes a code on the stack, returns the first char of the sequence it represents in bl
  push ebp
  mov ebp,esp
  xor eax,eax
  mov ax,[ebp+8]
  push eax
  call getTheSequence
  cmp eax,-1
  JNE haveChar;given code was in dict, so return its first char. otherwise return first char of previous code
  MOV esi,inPtr
  SUB esi,2
  xor eax,eax
  MOV ax,[esi]
  push eax  
  call getTheSequence 
  haveChar:
  XOR ebx,ebx
  MOV bl,[eax]
  pop ebp
  ret 4
firstChar ENDP
;--------------------------------------------------------------------------------------------------------------------
GetTheSequence PROC ;takes a code on the stack (DWORD), gives us eax=offset, ecx=length in bytes. eax = -1 means code not found
PUSH ebp
MOV ebp,esp

mov esi,ebp
add esi,8
mov eax,[esi]
;call writehex

xor eax,eax
MOV ax,[ebp+8]
add eax,eax
add eax,eax
add eax,eax
MOV ebx,dictIndex
CMP eax,EBX

JG notFound

mov ebx,eax
mov eax,dictKeys[ebx]
add ebx,4
mov ecx,dictKeys[ebx]

jmp donee

notFound:
mov eax,-1
donee:

POP ebp
ret 4
GetTheSequence ENDP
;--------------------------------------------------------------------------------------------------------
writeToOutput PROC ; takes offset at ebp+12,length at ebp+8. writes byte by byte to outfile
push ebp
mov ebp,esp
MOV esi,[ebp+12]
MOV ecx,[ebp+8]
MOV edi,outPtr
writeLoop:
  mov eax,[esi]
  mov [edi],eax
  inc edi
  inc esi
LOOP writeLoop
MOV outPtr,edi  
pop ebp
ret 8
writeToOutput ENDP
;-----------------------------------------------------------------------------------------------------------

END main