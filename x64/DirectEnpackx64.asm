format PE64 GUI 5.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win64w.inc'

prologue@proc equ static_rsp_prologue
epilogue@proc equ static_rsp_epilogue
close@proc equ static_rsp_close

COREL_REFRESH        =WM_USER+1
MAX_THREADS          =256
E_NOTIMPL            =80004001h
E_FAIL               =80004005h
SelectionChange      =11h
OnPluginCommand      =14h
OnUpdatePluginCommand=15h

struct TConfiguration
  BestPackCount        rd 1
  BestPack             rq 1
  PackedRectsCount     rd 1
  PackedRects          rq 1
  UnpackedRectsCount   rd 1
  UnpackedRects        rq 1
  MaxArea              rq 1
  Area                 rq 1
  CandidateRectsCount  rd 1
  CandidateRects       rq 1
  ConcaveCornersCount  rd 1
  ConcaveCorners       rq 1
  ConvexCornersCount   rd 1
  ConvexCorners        rq 1
  PackHeight           rd 1
  BestHeight           rd 1
ends

struct TCorner
  x     rd 1
  y     rd 1
  type  rd 1
  align rd 1
ends

struct TSize
  Width      rd 1
  Height     rd 1
  Angle      rd 1
  ShapeIndex rd 1
ends

struct TCandidate
  Left              rd 1
  Bottom            rd 1
  Right             rd 1
  Top               rd 1
  Shape             rq 1
  HalfPerimeter     rd 1
  Angle             rw 1
  UnpackedRectIndex rw 1
ends

include '..\Resources.inc'

IPlugin             dq IPluginVMT
IPluginVMT          dq QueryInterface,\
                       AddRef,\
                       Release,\
                       GetTypeInfoCount,\
                       GetTypeInfo,\
                       GetIDsOfNames,\
                       Invoke,\
                       OnLoad,\
                       StartSession,\
                       StopSession,\
                       OnUnload

include 'CorelDraw.inc'

varStrStandard  VARIANT VT_BSTR,strStandard
varInt          VARIANT VT_I4
dbl_00762       dq 0.0762

align 16
DllEntryPoint: ;hinstDLL,fdwReason,lpvReserved
  mov eax,TRUE
ret

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov qword[rcx],IPlugin
  mov eax,256
ret

BufStr2IntW:
  mov rdx,buf
  xor eax,eax
  jmp .start
      @@:movzx ecx,byte[rdx]
         lea   eax,[eax*4+eax]
         lea   eax,[eax*2+ecx-'0']
         add   rdx,2
  .start:cmp   word[rdx],0
  jne @b
ret

align 16
;rsi  - TConfiguration
;xmm0 - x,y
;Determining concave or convex corner by coordinates and place it in apropriate list
AddCorner:
  pxor xmm1,xmm1
  mov  rbp,[rsi+TConfiguration.PackedRects]
  movq xmm1,qword[Width]
  mov  eax,3
  xor  edx,edx
  .corners:
    movq     xmm2,qword[corners+rax*8]
    paddd    xmm2,xmm0
    add      edx,edx
    pshufd   xmm2,xmm2,01000100b
    movdqa   xmm3,xmm2
    pcmpgtd  xmm2,xmm1     ;(x<=0)or(y<=0)or(C.size[0]<=x)or(C.size[1]<=y)
    pmovmskb ecx,xmm2
    cmp      ecx,$FF00
    jne @f
    imul     ecx,[rsi+TConfiguration.PackedRectsCount],sizeof.TCandidate
    .ColissionCheck:
      movdqa   xmm2,dqword[rbp+rcx+TCandidate.Left-sizeof.TCandidate]
      pcmpgtd  xmm2,xmm3   ;(left<=x)and(bottom<=y)and(x<=right)and(y<=top)
      pmovmskb r8d,xmm2
      cmp      r8d,$FF00
      je @f
      sub      ecx,sizeof.TCandidate
    jne .ColissionCheck
    inc      edx
    @@:
    dec      eax
  jns .corners
  jmp qword[.case+rdx*8]
  .case: dq .quit,.concave,.concave,.quit,.concave,.quit,.quit,.convex,.concave,.quit,.quit,.convex,.quit,.convex,.convex,.quit
  .concave:imul eax,[rsi+TConfiguration.ConcaveCornersCount],sizeof.TCorner
           add  rax,[rsi+TConfiguration.ConcaveCorners]
           movq qword[rax+TCorner.x],xmm0
           mov  [rax+TCorner.type],edx
           inc  [rsi+TConfiguration.ConcaveCornersCount]
           ret
   .convex:imul eax,[rsi+TConfiguration.ConvexCornersCount],sizeof.TCorner
           add  rax,[rsi+TConfiguration.ConvexCorners]
           movq qword[rax+TCorner.x],xmm0
           mov  [rax+TCorner.type],edx
           inc  [rsi+TConfiguration.ConvexCornersCount]
     .quit:ret
ret

align 16
;rsi - TConfiguration
;rcx - TCandidate
;Placing rectangle in PackedRect list and rebuilding other lists (ConcaveCrorners, ConvexCorners, CandidateRects)
PlaceRect:
  mov    r9,[rsi+TConfiguration.PackedRects]
  mov    ebx,[rsi+TConfiguration.PackedRectsCount]
  mov    eax,[rcx+TCandidate.Right]
  mov    edx,[rcx+TCandidate.Top]
  shl    ebx,5
  sub    eax,[rcx+TCandidate.Left]
  sub    edx,[rcx+TCandidate.Bottom]
  movdqa xmm0,[rcx]
  movdqa xmm1,[rcx+16]
  mul    rdx
  movdqa [r9+rbx],xmm0
  movdqa [r9+rbx+16],xmm1
  add    [rsi+TConfiguration.Area],rax
  inc    [rsi+TConfiguration.PackedRectsCount]
  movzx  eax,[rcx+TCandidate.UnpackedRectIndex]
  mov    edx,[rsi+TConfiguration.UnpackedRectsCount]
  mov    r8,[rsi+TConfiguration.UnpackedRects]
  shl    eax,5
  shl    edx,4
  sub    [rsi+TConfiguration.UnpackedRectsCount],2
  movdqa xmm0,[r8+rdx-32]
  movdqa xmm1,[r8+rdx-16]
  movdqa [r8+rax],xmm0
  movdqa [r8+rax+16],xmm1
  mov    eax,[rsi+TConfiguration.PackHeight]
  mov    [rsi+TConfiguration.ConcaveCornersCount],0
  cmp    eax,[rcx+TCandidate.Top]
  mov    [rsi+TConfiguration.ConvexCornersCount],0
  cmovc  eax,[rcx+TCandidate.Top]
  mov    [rsi+TConfiguration.PackHeight],eax
  @@:movq   xmm0,qword[r9+rbx+TCandidate.Left]        ;(Left,Bottom)
     call   AddCorner
     pshufd xmm0,dqword[r9+rbx+TCandidate.Left],0110b ;(Right,Bottom)
     call   AddCorner
     pshufd xmm0,dqword[r9+rbx+TCandidate.Left],1100b ;(Left,Top)
     call   AddCorner
     movq   xmm0,qword[r9+rbx+TCandidate.Right]       ;(Right,Top)
     call   AddCorner
     sub    ebx,sizeof.TCandidate
  jns @b
;next folows FindCandidates routine

;rsi - TConfiguration
;Building candidate rectangle list, by placing rectangle in every concave corner
FindCandidates:
  mov      rdi,[rsi+TConfiguration.ConcaveCorners]
  mov      [rsi+TConfiguration.CandidateRectsCount],0
  pcmpeqd  xmm0,xmm0
  movdqa   xmm4,[mult+7*16]
  shufpd   xmm0,dqword[Width],0
  mov      rbx,[rsi+TConfiguration.UnpackedRects]
  mov      rbp,[rsi+TConfiguration.PackedRects]
  imul     r8d,[rsi+TConfiguration.ConcaveCornersCount],sizeof.TCorner
  test     r8,r8
  je       .quit
  .ConcaveCorners:
    mov      eax,[rdi+r8-sizeof.TCorner+TCorner.type]
    pshufd   xmm5,dqword[rdi+r8-sizeof.TCorner+TCorner.x],01000100b
    imul     edx,[rsi+TConfiguration.UnpackedRectsCount],sizeof.TSize
    shl      eax,4
    movdqa   xmm6,[mult+rax-16]
    jmp .start
    .unpackedRects:
      movq     xmm7,qword[rbx+rdx+TSize.Width]
      pshufd   xmm1,xmm7,01000100b
      pmulld   xmm1,xmm6
      paddd    xmm1,xmm5
      movdqa   xmm2,xmm1
      pcmpgtd  xmm2,xmm0     ;if (Candidate.Left>=0)and(Candidate.Bottom>=0)and(C.Width>=Candidate.Right)and(C.Height>=Candidate.Top) then
      pmovmskb eax,xmm2
      cmp      eax,$FF
      jne .Intersects
        pshufd xmm2,xmm1,01001110b
        paddd  xmm2,xmm4
        imul   ecx,[rsi+TConfiguration.PackedRectsCount],sizeof.TCandidate
        jecxz .ok
          @@:movdqa   xmm3,dqword[rbp+rcx-sizeof.TCandidate+TCandidate.Left]
             pcmpgtd  xmm3,xmm2
             pmovmskb eax,xmm3    ;if (Candidate.Right>Left)and(Right>Candidate.Left)and(Candidate.Top>Bottom)and(Top>Candidate.Bottom) then
             cmp      eax,$FF00
             je .Intersects
             sub      ecx,sizeof.TCandidate
          jne @b
        .ok:
        cvtsi2ss  xmm3,edx
        imul      ecx,[rsi+TConfiguration.CandidateRectsCount],sizeof.TCandidate
        mulss     xmm3,[rcp_sizeof_unpackedRect]
        add       rcx,[rsi+TConfiguration.CandidateRects]
        cvttss2si eax,xmm3
        movdqa    dqword[rcx+TCandidate.Left],xmm1
        phaddd    xmm7,xmm7
        movd      [rcx+TCandidate.HalfPerimeter],xmm7
        mov       [rcx+TCandidate.UnpackedRectIndex],ax
        mov       eax,[rbx+rdx+TSize.Angle]
        inc       [rsi+TConfiguration.CandidateRectsCount]
        mov       [rcx+TCandidate.Angle],ax
        mov       eax,[rbx+rdx+TSize.ShapeIndex]
        mov       rax,[r13+rax*8]
        mov       [rcx+TCandidate.Shape],rax
      .Intersects:
      .start:
      sub      edx,sizeof.TSize
    jns .unpackedRects
    sub r8,sizeof.TCorner
  jne .ConcaveCorners
  .quit:
ret

align 16
proc Search ;(ThreadIndex: dword);stdcall;
  mov       r13,[Shapes]
  mov       r14,rcx
  imul      esi,ecx,sizeof.TConfiguration
  add       rsi,Cxx
  .TrhreadLoop:
    invoke    WaitForSingleObject,[InThreadEvents+r14*8],-1 ;INFINITE

    mov       [rsi+TConfiguration.MaxArea],0
    mov       [rsi+TConfiguration.BestHeight],0
    mov       eax,[C.CandidateRectsCount]
    cdq
    div       [ThreadCount]
    mov       r11,rax
    imul      eax,r14d
    cmp       r14,rdx
    cmovc     rdx,r14
    adc       r11,0             ;QuantSize
    lea       r10,[rax+rdx]     ;QuantStart

    cmp r10d,[C.CandidateRectsCount]
    jnc .continue
      add       r11,r10
      cmp       r11d,[C.CandidateRectsCount]
      cmova     r11d,[C.CandidateRectsCount]
      sub       r11,r10
      shl       r11,5
      shl       r10,5
      add       r10,[C.CandidateRects]

      mov       eax,[C.PackedRectsCount]                     ;eax may contains 0, but it`s ok
      mov       rdx,[C.PackedRects]
      mov       rcx,[rsi+TConfiguration.PackedRects]
      shl       eax,5
      @@:sub    rax,sizeof.TCandidate
         movdqa xmm0,[rdx+rax]
         movdqa xmm1,[rdx+rax+16]
         movdqa [rcx+rax],xmm0
         movdqa [rcx+rax+16],xmm1
      jnle @b

      .CandidateTry:
        cmp       [BreakPacking],0
        jne       .quit

        mov       r8,[C.Area]
        mov       ecx,[C.PackedRectsCount]                   ;ecx may contains 0, but it`s ok
        mov       [rsi+TConfiguration.PackedRectsCount],ecx
        mov       rax,[C.PackedRects]
        mov       rdx,[rsi+TConfiguration.PackedRects]
        shl       ecx,5
        movdqa    xmm0,[rax+rcx-32]
        movdqa    xmm1,[rax+rcx-16]
        movdqa    [rdx+rcx-32],xmm0
        movdqa    [rdx+rcx-16],xmm1
        mov       eax,[C.UnpackedRectsCount]
        mov       edx,[C.PackHeight]
        mov       [rsi+TConfiguration.Area],r8
        mov       [rsi+TConfiguration.UnpackedRectsCount],eax
        mov       [rsi+TConfiguration.PackHeight],edx
        mov       rdx,[C.UnpackedRects]
        mov       rcx,[rsi+TConfiguration.UnpackedRects]

        shl       eax,4
        @@:sub    eax,16
           movdqa xmm0,[rdx+rax]
           movdqa [rcx+rax],xmm0
        jne @b

        lea  rcx,[r10+r11-sizeof.TCandidate]
        jmp .start
        .BuildConfiguration:
            movss  xmm3,[maxrate]
            pxor   xmm1,xmm1
            imul   ebx,[rsi+TConfiguration.CandidateRectsCount],sizeof.TCandidate
            .FindBest:
              mov    rdi,[rsi+TConfiguration.CandidateRects]
              mov    ecx,4
              pshufd xmm0,dqword[rdi+rbx+TCandidate.Left-sizeof.TCandidate],11011000b
              imul   edx,[rsi+TConfiguration.ConvexCornersCount],sizeof.TCorner
              test   edx,edx
              je @f
                mov    rdi,[rsi+TConfiguration.ConvexCorners]
                .convex:
                  pshufd   xmm2,dqword[rdi+rdx-sizeof.TCorner+TCorner.x],01010000b
                  pcmpeqd  xmm2,xmm0
                  pcmpeqq  xmm2,xmm1
                  pmovmskb eax,xmm2
                  cmp      eax,1
                  sbb      ecx,0
                  sub      edx,sizeof.TCorner
                jne .convex
              @@:
              imul   edx,[rsi+TConfiguration.ConcaveCornersCount],sizeof.TCorner
              mov    rdi,[rsi+TConfiguration.ConcaveCorners]
              .concave:
                pshufd   xmm2,dqword[rdi+rdx-sizeof.TCorner+TCorner.x],01010000b
                pcmpeqd  xmm2,xmm0
                pcmpeqq  xmm2,xmm1
                pmovmskb eax,xmm2
                cmp      eax,1
                sbb      ecx,0
                sub      edx,sizeof.TCorner
              jne .concave
              mov      rdi,[rsi+TConfiguration.CandidateRects]
              shl      ecx,cl
              imul     ecx,[rdi+rbx-sizeof.TCandidate+TCandidate.Top]
              cvtsi2ss xmm0,ecx
              cvtsi2ss xmm2,[rdi+rbx-sizeof.TCandidate+TCandidate.HalfPerimeter]
              divss    xmm0,xmm2
              comiss   xmm0,xmm3
              minss    xmm3,xmm0
              cmovc    ebp,ebx
              sub      ebx,sizeof.TCandidate
            jne .FindBest
            lea    ecx,[ebp-32]
            add    rcx,[rsi+TConfiguration.CandidateRects]
        .start:
            call   PlaceRect
            cmp    [rsi+TConfiguration.CandidateRectsCount],0
        ja .BuildConfiguration
        mov  eax,[rsi+TConfiguration.PackHeight]
        mov  edx,[rsi+TConfiguration.BestHeight]
        add  rax,[rsi+TConfiguration.MaxArea]
        add  rdx,[rsi+TConfiguration.Area]
        cmp  rdx,rax
        jbe .below
          mov rax,[rsi+TConfiguration.Area]
          mov ecx,[rsi+TConfiguration.PackHeight]
          mov edx,[rsi+TConfiguration.PackedRectsCount]
          mov [rsi+TConfiguration.MaxArea],rax
          mov [rsi+TConfiguration.BestHeight],ecx
          mov [rsi+TConfiguration.BestPackCount],edx
          mov rax,[rsi+TConfiguration.PackedRects]
          mov rcx,[rsi+TConfiguration.BestPack]
          shl edx,5
          @@:movdqa xmm0,[rax+rdx-32]
             movdqa xmm1,[rax+rdx-16]
             movdqa [rcx+rdx-32],xmm0
             movdqa [rcx+rdx-16],xmm1
             sub    edx,32
          jne @b
        .below:
        sub r11,sizeof.TCandidate
      jne .CandidateTry
    .continue:
    invoke SetEvent,[OutThreadEvents+r14*8]
    cmp    [BreakPacking],0
  je .TrhreadLoop
  .quit:
  invoke CloseHandle,[InThreadEvents+r14*8]
  invoke SetEvent,[OutThreadEvents+r14*8]
  invoke CloseHandle,[OutThreadEvents+r14*8]
ret
endp

align 16
;Main packing thread
proc Pack
  invoke   GetSystemInfo,SysInfo
  cominvk  CorelApp,Get_ActiveSelectionRange,Selection
  cominvk  Selection,GetPosition,Origin.x,Origin.y
  cominvk  Selection,Get_Count,ShapesCount
  invoke   SendDlgItemMessageW,[DialogWindow],7,PBM_SETRANGE32,0,[ShapesCount]

  cvtsi2sd xmm0,[Span]
  movddup  xmm0,xmm0
  mulpd    xmm0,[dbl_05]
  addpd    xmm0,[Origin]
  movapd   [Origin],xmm0

  movq     xmm0,qword[SCALE_FACTOR]
  pmulld   xmm0,dqword[Width]
  movq     qword[Width],xmm0

  mov      ebx,MAX_THREADS
  cmp      ebx,[SysInfo.dwNumberOfProcessors]
  cmova    ebx,[SysInfo.dwNumberOfProcessors]
  mov      [ThreadCount],ebx

;;;;;;;;;;;;;;;    Memory allocation and treads/events creation    ;;;;;;;;;;;;;;;
  .InitConfigurations:
    imul   eax,ebx,sizeof.TConfiguration
    lea    rsi,[C+rax]
    mov    [rsi+TConfiguration.PackedRectsCount],0
    mov    [rsi+TConfiguration.BestPackCount],0
    mov    [rsi+TConfiguration.PackHeight],0
    mov    [rsi+TConfiguration.Area],0
    mov    eax,[ShapesCount]
    mov    edx,eax
    imul   edx,eax
    shl    edx,8                                   ;ShapesCount*ShapesCount*8*sizeof.TCandidate
    mov    [rsi+TConfiguration.PackedRects],rdx
    shl    eax,1
    mov    [rsi+TConfiguration.UnpackedRectsCount],eax
    shl    eax,4
    add    edx,eax                                 ;+ShapesCount*sizeof.TCandidate
    mov    [rsi+TConfiguration.BestPack],rdx
    add    edx,eax                                 ;+ShapesCount*sizeof.TCandidate
    mov    [rsi+TConfiguration.ConcaveCorners],rdx
    lea    edx,[edx+eax*2]                         ;+ShapesCount*sizeof.TCorner*4
    mov    [rsi+TConfiguration.ConvexCorners],rdx
    lea    edx,[edx+eax*2]                         ;+ShapesCount*sizeof.TCorner*4
    mov    [rsi+TConfiguration.UnpackedRects],rdx
    add    edx,eax                                 ;+ShapesCount*sizeof.TSize*2
    invoke VirtualAlloc,0,rdx,MEM_COMMIT,PAGE_READWRITE
    mov    [rsi+TConfiguration.CandidateRects],rax
    add    [rsi+TConfiguration.PackedRects],rax
    add    [rsi+TConfiguration.BestPack],rax
    add    [rsi+TConfiguration.ConcaveCorners],rax
    add    [rsi+TConfiguration.ConvexCorners],rax
    add    [rsi+TConfiguration.UnpackedRects],rax
    dec    ebx
    js @f
      invoke CreateEventW,0,0,0,0
      mov    [InThreadEvents+rbx*8],rax
      invoke CreateEventW,0,0,0,0
      mov    [OutThreadEvents+rbx*8],rax
      invoke CreateThread,0,0,Search,rbx,0,addr ThreadIds+rbx*8
  jmp .InitConfigurations
  @@:

;;;;;;;;;;;;;;;    Getting shapes from CorelDraw    ;;;;;;;;;;;;;;;
  cominvk CorelApp,Set_Optimization,1
  mov     ebx,[ShapesCount]
  mov     edi,ebx
  shl     edi,5
  add     rdi,[C.UnpackedRects]
  lea     r12,[rbx*8+15]
  and     r12,-16
  sub     rsp,r12
  lea     r13,[rsp+framebytes@proc]   ; reserve memory for IVGShape, so r11 would point to Shapes array
  mov     [Shapes],r13
  @@:sub      rdi,sizeof.TSize*2
     mov      [varInt.data],rbx
     lea      r8,[r13+rbx*8-8]
     cominvk  Selection,Get_Item,varInt,r8
     mov      rbp,[r13+rbx*8-8]
     pxor     xmm1,xmm1
     comcall  rbp,IVGShape,Set_RotationAngle,xmm1
     comcall  rbp,IVGShape,Get_SizeWidth,buf
     comcall  rbp,IVGShape,Get_SizeHeight,buf+8
     cvtsi2sd xmm0,[Span]
     cvtpi2pd xmm1,qword[SCALE_FACTOR]
     movddup  xmm0,xmm0
     addpd    xmm0,dqword[buf]
     mulpd    xmm0,xmm1
     cvtpd2dq xmm0,xmm0
     movq     qword[rdi+TSize.Width],xmm0
     lea      rax,[rbx-1]
     mov      [rdi+TSize.ShapeIndex],eax
     pshufd   xmm0,xmm0,1
     movq     qword[rdi+sizeof.TSize+TSize.Width],xmm0
     mov      [rdi+sizeof.TSize+TSize.ShapeIndex],eax
     mov      [rdi+sizeof.TSize+TSize.Angle],90
     dec      ebx
  jne @b
  cominvk Selection,Release

;;;;;;;;;;;;;;;    Random shapes shuffle (if selected)    ;;;;;;;;;;;;;;;
  invoke SendDlgItemMessageW,[DialogWindow],4,BM_GETSTATE,0,0
  test   eax,BST_CHECKED
  je @f
    mov ecx,[ShapesCount]
    mov rbx,[C.UnpackedRects]
    shl ecx,5
    .Randomize:
      rdtsc
      mul    edx            ;I don`t want use crc32 or rdrand because old cpu compatibility
      xor    edx,edx
      div    [ShapesCount]
      shl    edx,5
      sub    ecx,sizeof.TSize*2
      movdqa xmm0,[rbx+rdx]
      movdqa xmm1,[rbx+rdx+sizeof.TSize]
      movdqa xmm2,[rbx+rcx]
      movdqa xmm3,[rbx+rcx+sizeof.TSize]
      movdqa [rbx+rcx],xmm0
      movdqa [rbx+rcx+sizeof.TSize],xmm1
      movdqa [rbx+rdx],xmm2
      movdqa [rbx+rdx+sizeof.TSize],xmm3
    jne .Randomize
  @@:

;;;;;;;;;;;;;;;    Rectangle packing routine    ;;;;;;;;;;;;;;;
  mov    rax,[C.ConcaveCorners]
  mov    qword[rax+TCorner.x],0
  mov    [rax+TCorner.type],1 ;Bottom left corner
  mov    [C.ConcaveCornersCount],1
  mov    rsi,C
  call   FindCandidates
  mov    [C.MaxArea],0
  mov    [C.BestHeight],0
  jmp .StartPacking
  .RectPack:
    mov ebx,[ThreadCount]
    @@:invoke SetEvent,[InThreadEvents+rbx*8-8]
       dec    ebx
    jne @b
    cmp    [BreakPacking],0
    jne .quit
    invoke WaitForMultipleObjects,[ThreadCount],OutThreadEvents,1,-1

    imul   edi,[ThreadCount],sizeof.TConfiguration
    xor    ebp,ebp
    .FindBest:
       mov  rax,[C.MaxArea]
       mov  ebx,[C.BestHeight+edi]
       mov  rcx,[C.MaxArea+rdi]
       mov  edx,[C.BestHeight]
       add  rax,rbx
       add  rcx,rdx
       cmp  rax,rcx
       jae  @f
         mov ebp,edi
         sub rcx,rdx
         mov [C.MaxArea],rcx
         mov [C.BestHeight],ebx
       @@:
       sub edi,sizeof.TConfiguration
    jne .FindBest

    test ebp,ebp
    je .NotFound
      cominvk CorelApp,Set_Optimization,1
      mov     ebx,[ShapesCount]
      @@:mov      eax,[Width]
         add      eax,UNPACKED_OFFSET
         xorpd    xmm1,xmm1
         cvtsi2sd xmm1,eax
         mulsd    xmm1,[RCP_SCALE_FACTOR]
         addsd    xmm1,[Origin.x]
         movsd    xmm2,[Origin.y]
         comcall  qword[r13+rbx*8-8],IVGShape,SetPosition,xmm1,xmm2
         dec      ebx
      jne @b
      mov ebx,[C.BestPackCount+rbp]
      mov rbp,[C.BestPack+rbp]
      mov rdi,[C.BestPack]
      mov [C.BestPackCount],ebx
      shl ebx,5
      @@:movdqa   xmm0,[rbp+rbx-32]
         movdqa   xmm1,[rbp+rbx-16]
         movdqa   [rdi+rbx-32],xmm0
         movdqa   [rdi+rbx-16],xmm1
         movzx    eax,[rbp+rbx-sizeof.TCandidate+TCandidate.Angle]
         cvtsi2sd xmm1,eax
         comcall  [rbp+rbx-sizeof.TCandidate+TCandidate.Shape],IVGShape,Set_RotationAngle,xmm1
         cvtpi2pd xmm1,qword[rbp+rbx-sizeof.TCandidate+TCandidate.Left]
         mulpd    xmm1,[RCP_SCALE_FACTOR]
         addpd    xmm1,[Origin]
         movhlps  xmm2,xmm1
         comcall  [rbp+rbx-sizeof.TCandidate+TCandidate.Shape],IVGShape,SetPosition,xmm1,xmm2
         sub      ebx,sizeof.TCandidate
      jne @b
      cominvk  CorelApp,Set_Optimization,0
      cmp      [BreakPacking],0
      jne @f
        cvtsi2sd xmm0,[C.BestHeight]
        mulsd    xmm0,[RCP_SCALE_FACTOR]
        cvtsd2si eax,xmm0
        cinvoke  wsprintfW,buf,fmt,[C.BestPackCount],[ShapesCount],rax
        invoke   SendMessageW,[DialogWindow],COREL_REFRESH,0,0
        invoke   SendMessageW,[DialogWindow],WM_SETTEXT,0,buf
      @@:
    .NotFound:

    invoke  PostMessageW,[ProgressBar],PBM_STEPIT,0,0
    mov     ecx,[C.PackedRectsCount]
    shl     ecx,5
    add     rcx,[C.BestPack]
    mov     rsi,C
    call    PlaceRect
  .StartPacking:
    cmp [C.CandidateRectsCount],0
  ja .RectPack
  .quit:
  lock inc [BreakPacking]
  cmp [BreakPacking],1
  je .RectPack

;;;;;;;;;;;;;;;    Draw rectangle frames (if selected)    ;;;;;;;;;;;;;;;
  cmp [DrawFrames],0
  je @f
    cominvk  CorelApp,Set_Optimization,1
    cominvk  CorelDoc,Get_ActiveLayer,Layer
    cvtsi2sd xmm1,[Span]
    movapd   xmm0,[Origin]
    movddup  xmm1,xmm1
    mulpd    xmm1,[dbl_05]
    subpd    xmm0,xmm1
    movapd   [Origin],xmm0
    mov      ebx,[C.BestPackCount]
    mov      rbp,[C.BestPack]
    shl      ebx,5
    .DrawFrames:
      cvtpi2pd xmm1,qword[rbp+rbx-sizeof.TCandidate+TCandidate.Left]
      cvtpi2pd xmm3,qword[rbp+rbx-sizeof.TCandidate+TCandidate.Right]
      mulpd    xmm1,[RCP_SCALE_FACTOR]
      mulpd    xmm3,[RCP_SCALE_FACTOR]
      addpd    xmm1,[Origin]
      addpd    xmm3,[Origin]
      movhlps  xmm2,xmm1
      movhlps  xmm4,xmm3
      cominvk  Layer,CreateRectangle,xmm1,xmm2,xmm3,xmm4,0,0,0,0,Shape
      cominvk  Shape,Get_Outline,Outline
      cominvk  Outline,Set_Width,float [dbl_00762]
      cominvk  Outline,Release
      cominvk  Shape,Release
      sub      ebx,sizeof.TCandidate
    jne .DrawFrames
    cominvk  Layer,Release
  @@:

;;;;;;;;;;;;;;;    Free resources    ;;;;;;;;;;;;;;;
  mov   ebx,[ShapesCount]
  @@:comcall qword[r13+rbx*8-8],IVGShape,Release
     dec     ebx
  jne @b
  add     rsp,r12
  cominvk CorelApp,Set_Optimization,0
  imul    ebx,[ThreadCount],sizeof.TConfiguration
  @@:invoke VirtualFree,[C+rbx+TConfiguration.CandidateRects],0,MEM_RELEASE
     sub    ebx,sizeof.TConfiguration
  jns @b
  invoke  PostMessageW,[DialogWindow],WM_CLOSE,0,0
ret
endp

align 16
proc DialogFunc wnd,msg,wParam,lParam
  cmp edx,COREL_REFRESH
  je .COREL_REFRESH
  cmp edx,WM_COMMAND
  je .WM_COMMAND
  cmp edx,WM_INITDIALOG
  je .WM_INITDIALOG
  cmp edx,WM_CLOSE
  je .WM_CLOSE
    xor eax,eax
  ret
  .COREL_REFRESH:cominvk CorelApp,Refresh
                 ret
     .WM_COMMAND:cmp r8,(BN_CLICKED shl 16)+5
                 je  .DrawFramesChange
                 cmp r8,(BN_CLICKED shl 16)+6
                 jne @f
                   mov    [BreakPacking],0
                   invoke EnableWindow,r9,0
                   invoke SendDlgItemMessageW,[DialogWindow],1,WM_GETTEXT,bufsize/2,buf
                   call   BufStr2IntW
                   mov    [Width],eax
                   invoke SendDlgItemMessageW,[DialogWindow],2,WM_GETTEXT,bufsize/2,buf
                   call   BufStr2IntW
                   mov    [Height],eax
                   invoke SendDlgItemMessageW,[DialogWindow],3,WM_GETTEXT,bufsize/2,buf
                   call   BufStr2IntW
                   mov    [Span],eax
                   invoke CreateThread,0,4096*1024,Pack,0,0,0
                   mov    [PackTreadHandle],rax
                   .DrawFramesChange:
                   invoke SendDlgItemMessageW,[DialogWindow],5,BM_GETSTATE,0,0
                   and    eax,BST_CHECKED
                   mov    [DrawFrames],eax
                 @@:
                 ret
  .WM_INITDIALOG:mov    [DrawFrames],0
                 mov    [DialogWindow],rcx
                 invoke SendDlgItemMessageW,rcx,1,EM_SETLIMITTEXT,5,0
                 invoke SendDlgItemMessageW,[DialogWindow],2,EM_SETLIMITTEXT,5,0
                 invoke SendDlgItemMessageW,[DialogWindow],3,EM_SETLIMITTEXT,3,0
                 invoke GetDlgItem,[DialogWindow],7
                 mov    [ProgressBar],rax
                 invoke PostMessageW,rax,PBM_SETSTEP,1,0
                 ret
       .WM_CLOSE:lock inc [BreakPacking]
                 invoke WaitForSingleObject,[PackTreadHandle],-1;INFINITE
                 invoke EndDialog,[DialogWindow],0
                 ret
endp

QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov qword[r8],IPlugin
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
GetIDsOfNames:    ; this,IID,Names,NameCount,LocaleID,DispIDs
  mov eax,E_NOTIMPL
ret

proc Invoke this,DispID,IID,LocaleID,Flags,Params,VarResult,ExcepInfo,ArgErr
  cmp edx,SelectionChange
  je .SelectionChange
  cmp edx,OnPluginCommand
  je .OnPluginCommand
  cmp edx,OnUpdatePluginCommand
  je .OnUpdatePluginCommand
  xor eax,eax
  ret
        .SelectionChange:cominvk CorelApp,Get_ActiveSelectionRange,Selection
                         test    eax,eax
                         jne @f
                           cominvk Selection,Get_Count,Enabled
                           cominvk Selection,Release
                         @@:
                         xor     eax,eax
                         ret
        .OnPluginCommand:mov    rax,[Params]
                         mov    rax,[rax+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,[rax+VARIANT.data],strDirectEnpack
                         test   eax,eax
                         jne    @f
                           cominvk CorelApp,Get_ActiveDocument,CorelDoc
                           cominvk CorelDoc,BeginCommandGroup,0
                           cominvk CorelDoc,Set_Unit,cdrMillimeter
                           cominvk CorelDoc,Set_ReferencePoint,cdrBottomLeft
                           cominvk CorelApp,Get_ActiveWindow,CorelWnd
                           cominvk CorelWnd,Get_Handle,CorelWndHandle
                           cominvk CorelWnd,Release
                           invoke  DialogBoxIndirectParamW,0,MainDlg,[CorelWndHandle],DialogFunc,0
                           cominvk CorelDoc,EndCommandGroup ;this method hangs up if calls from another thread, so i do it here
                           cominvk CorelDoc,Release
                           cominvk CorelApp,Refresh
                         @@:
                         xor     eax,eax
                         ret
  .OnUpdatePluginCommand:xchg   rbx,[Params]
                         mov    rbx,[rbx+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,[rbx+sizeof.VARIANT*2+VARIANT.data],strDirectEnpack
                         test   eax,eax
                         jne    @f
                           mov rax,[rbx+sizeof.VARIANT*1+VARIANT.data]
                           mov edx,[Enabled]
                           mov [rax],dx
                         @@:
                         mov    rbx,[Params]
                         xor    eax,eax
                         ret
endp

proc OnLoad uses rbx ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  mov     rbx,rdx
  mov     [CorelApp],rdx
  comcall rbx,IVGApplication,AddRef
  comcall rbx,IVGApplication,Get_VersionMinor,CorelVersion
  comcall rbx,IVGApplication,Get_VersionMajor,addr CorelVersion+1
ret
endp

proc StartSession uses rbx     ;(const self:IVGAppPlugin):LongInt;stdcall;
  mov     eax,1
  cpuid
  test    ecx,1 shl 19 ;SSE 4.1
  je .CPUNotSupported
    mov     rbx,[CorelApp]
    comcall rbx,IVGApplication,AddPluginCommand,strDirectEnpack,strButtonCaption,strButtonCaption,buf
    comcall rbx,IVGApplication,AdviseEvents,IPlugin,EventsCookie
    comcall rbx,IVGApplication,Get_CommandBars,CommandBars
    cominvk CommandBars,Get_Item,varStrStandard,CommandBar
    cominvk CommandBar,Get_Controls,Controls
    cominvk Controls,Get_Count,buf
    mov     ebx,dword[buf]
    @@:cominvk Controls,Get_Item,rbx,Control
       cominvk Control,Get_Caption,buf
       invoke  lstrcmpW,qword[buf],strButtonCaption
       test    eax,eax
       je @f
       cominvk Control,Release
       dec     ebx
    jne @b
    cominvk Controls,AddCustomButton,cdrCmdCategoryPlugins,strDirectEnpack,1,0,Control
    @@:
    invoke  GetTempPathW,bufsize/2,buf+4
    lea     eax,[eax*2+sizeof.strDirectEnpack+6]
    mov     dword[buf],eax
    lea     rcx,[buf+eax-sizeof.strDirectEnpack-2]
    invoke  lstrcpyW,rcx,strDirectEnpack
    cmp    [CorelVersion],1104h ;17.4
    jb .legacy
      mov     rdx,6F00630069002Eh                 ;'.ico'
      mov     [rax+sizeof.strDirectEnpack-2],rdx
      invoke  CreateFileW,buf+4,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0
      mov     rbx,rax
      invoke  WriteFile,rax,ICOData,sizeof.ICOData,Span,0
      invoke  CloseHandle,rbx
      cominvk Control,SetIcon2,buf+4
      jmp @f
    .legacy:
      mov     rdx,70006D0062002Eh                 ;'.bmp'
      mov     [rax+sizeof.strDirectEnpack-2],rdx
      invoke  CreateFileW,buf+4,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0
      mov     rbx,rax
      invoke  WriteFile,rax,BMPData,sizeof.BMPData,Span,0
      invoke  CloseHandle,rbx
      cominvk Control,SetCustomIcon,buf+4
    @@:
    cominvk Control,Release
    cominvk Controls,Release
    cominvk CommandBar,Release
    cominvk CommandBars,Release
    xor     eax,eax
    ret
  .CPUNotSupported:
  invoke MessageBoxW,[CorelWndHandle],errCPUNotSupported,strDirectEnpack,MB_TASKMODAL
  mov    eax,E_FAIL
ret
endp

proc StopSession      ;(const self:IVGAppPlugin):LongInt;stdcall;
  cominvk CorelApp,UnadviseEvents,[EventsCookie]
  xor     eax,eax
ret
endp

proc OnUnload         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CorelApp,Release
  xor     eax,eax
ret
endp

align 16
buf             rb 512
bufsize=$-buf
Origin:
  .x            rq 1
  .y            rq 1
Width           rd 1
Height          rd 1
Span            rd 1
DrawFrames      rd 1
InThreadEvents  rq MAX_THREADS
OutThreadEvents rq MAX_THREADS
ThreadIds       rq MAX_THREADS
CorelApp        IVGApplication
CorelDoc        IVGDocument
CommandBars     ICUICommandBars
CommandBar      ICUICommandBar
Controls        ICUIControls
Control         ICUIControl
CorelWnd        IVGWindow
Layer           IVGLayer
Shape           IVGShape
Outline         IVGOutline
Selection       IVGShapeRange
Shapes          rq 1
PackTreadHandle rq 1
ProgressBar     rq 1
CorelWndHandle  rq 1
DialogWindow    rq 1
C               TConfiguration
Cxx             rb MAX_THREADS*sizeof.TConfiguration
BreakPacking    rd 1
ShapesCount     rd 1
ThreadCount     rd 1
EventsCookie    rd 1
Enabled         rd 1
CorelVersion    rd 1
SysInfo         SYSTEM_INFO