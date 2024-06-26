format PE GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win32w.inc'

COREL_REFRESH        =WM_USER+1
MAX_THREADS          =256
E_NOTIMPL            =80004001h
E_FAIL               =80004005h
SelectionChange      =11h
OnPluginCommand      =14h
OnUpdatePluginCommand=15h

struct TConfiguration
  BestPackCount        rd 1
  BestPack             rd 1
  PackedRectsCount     rd 1
  PackedRects          rd 1
  UnpackedRectsCount   rd 1
  UnpackedRects        rd 1
  MaxArea              rq 1
  Area                 rq 1
  CandidateRectsCount  rd 1
  CandidateRects       rd 1
  ConcaveCornersCount  rd 1
  ConcaveCorners       rd 1
  ConvexCornersCount   rd 1
  ConvexCorners        rd 1
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
  Width  rd 1
  Height rd 1
  Angle  rd 1
  Shape  rd 1
ends

struct TCandidate
  Left              rd 1
  Bottom            rd 1
  Right             rd 1
  Top               rd 1
  Shape             rd 1
  HalfPerimeter     rd 1
  Angle             rw 1
  UnpackedRectIndex rw 1
  align             rd 1
ends

include '..\Resources.inc'

IPlugin             dd IPluginVMT
IPluginVMT          dd QueryInterface,\
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

align 16
DllEntryPoint: ;hinstDLL,fdwReason,lpvReserved
  mov eax,TRUE
ret 12

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov eax,[esp+4]
  mov dword[eax],IPlugin
  mov eax,256
ret 4

BufStr2IntW:
  mov edx,buf
  xor eax,eax
  jmp .start
      @@:movzx ecx,byte[edx]
         lea   eax,[eax*4+eax]
         lea   eax,[eax*2+ecx-'0']
         add   edx,2
  .start:cmp   word[edx],0
  jne @b
ret

align 16
;esi - TConfiguration
;Building candidate rectangle list, by placing rectangle in every concave corner
FindCandidates:
  mov      edi,[esi+TConfiguration.ConcaveCorners]
  mov      [esi+TConfiguration.CandidateRectsCount],0
  pcmpeqd  xmm0,xmm0
  movdqa   xmm4,[mult+7*16]
  shufpd   xmm0,dqword[Width],0
  mov      ebx,[esi+TConfiguration.UnpackedRects]
  mov      ebp,[esi+TConfiguration.PackedRects]
  imul     ecx,[esi+TConfiguration.ConcaveCornersCount],sizeof.TCorner
  test     ecx,ecx
  je       .quit
  .ConcaveCorners:
    mov      eax,[edi+ecx-sizeof.TCorner+TCorner.type]
    pshufd   xmm5,dqword[edi+ecx-sizeof.TCorner+TCorner.x],01000100b
    imul     edx,[esi+TConfiguration.UnpackedRectsCount],sizeof.TSize
    shl      eax,4
    movdqa   xmm6,[mult+eax-16]
    jmp .start
    .unpackedRects:
      movq     xmm7,qword[ebx+edx+TSize.Width]
      pshufd   xmm1,xmm7,01000100b
      pmulld   xmm1,xmm6
      paddd    xmm1,xmm5
      movdqa   xmm2,xmm1
      pcmpgtd  xmm2,xmm0     ;if (Candidate.Left>=0)and(Candidate.Bottom>=0)and(C.Width>=Candidate.Right)and(C.Height>=Candidate.Top) then
      pmovmskb eax,xmm2
      cmp      eax,$FF
      push     ecx
      jne .Intersects
        pshufd xmm2,xmm1,01001110b
        paddd  xmm2,xmm4
        imul   ecx,[esi+TConfiguration.PackedRectsCount],sizeof.TCandidate
        jecxz .ok
          @@:movdqa   xmm3,dqword[ebp+ecx-sizeof.TCandidate+TCandidate.Left]
             pcmpgtd  xmm3,xmm2
             pmovmskb eax,xmm3    ;if (Candidate.Right>Left)and(Right>Candidate.Left)and(Candidate.Top>Bottom)and(Top>Candidate.Bottom) then
             cmp      eax,$FF00
             je .Intersects
             sub      ecx,sizeof.TCandidate
          jne @b
        .ok:
        cvtsi2ss  xmm3,edx
        imul      ecx,[esi+TConfiguration.CandidateRectsCount],sizeof.TCandidate
        mulss     xmm3,[rcp_sizeof_unpackedRect]
        add       ecx,[esi+TConfiguration.CandidateRects]
        cvttss2si eax,xmm3
        movdqa    dqword[ecx+TCandidate.Left],xmm1
        phaddd    xmm7,xmm7
        movd      [ecx+TCandidate.HalfPerimeter],xmm7
        mov       [ecx+TCandidate.UnpackedRectIndex],ax
        mov       eax,[ebx+edx+TSize.Angle]
        inc       [esi+TConfiguration.CandidateRectsCount]
        mov       [ecx+TCandidate.Angle],ax
        mov       eax,[ebx+edx+TSize.Shape]
        mov       [ecx+TCandidate.Shape],eax
      .Intersects:
      pop      ecx
      .start:
      sub      edx,sizeof.TSize
    jns .unpackedRects
    sub ecx,sizeof.TCorner
  jne .ConcaveCorners
  .quit:
ret

align 16
;esi  - TConfiguration
;xmm0 - x,y
;Determining concave or convex corner by coordinates and place it in apropriate list
AddCorner:
  push edi
  pxor xmm1,xmm1
  mov  ebp,[esi+TConfiguration.PackedRects]
  movq xmm1,qword[Width]
  mov  eax,3
  xor  edx,edx
  .corners:
    movq     xmm2,qword[corners+eax*8]
    paddd    xmm2,xmm0
    add      edx,edx
    pshufd   xmm2,xmm2,01000100b
    movdqa   xmm3,xmm2
    pcmpgtd  xmm2,xmm1     ;(x<=0)or(y<=0)or(C.size[0]<=x)or(C.size[1]<=y)
    pmovmskb ecx,xmm2
    cmp      ecx,$FF00
    jne @f
    imul     ecx,[esi+TConfiguration.PackedRectsCount],sizeof.TCandidate
    .ColissionCheck:
      movdqa   xmm2,dqword[ebp+ecx+TCandidate.Left-sizeof.TCandidate]
      pcmpgtd  xmm2,xmm3   ;(left<=x)and(bottom<=y)and(x<=right)and(y<=top)
      pmovmskb edi,xmm2
      cmp      edi,$FF00
      je @f
      sub      ecx,sizeof.TCandidate
    jne .ColissionCheck
    inc      edx
    @@:
    dec      eax
  jns .corners
  jmp [.case+edx*4]
  .case dd .quit,.concave,.concave,.quit,.concave,.quit,.quit,.convex,.concave,.quit,.quit,.convex,.quit,.convex,.convex,.quit
  .concave:imul eax,[esi+TConfiguration.ConcaveCornersCount],sizeof.TCorner
           add  eax,[esi+TConfiguration.ConcaveCorners]
           movq qword[eax+TCorner.x],xmm0
           mov  [eax+TCorner.type],edx
           inc  [esi+TConfiguration.ConcaveCornersCount]
           jmp .quit
   .convex:imul eax,[esi+TConfiguration.ConvexCornersCount],sizeof.TCorner
           add  eax,[esi+TConfiguration.ConvexCorners]
           movq qword[eax+TCorner.x],xmm0
           mov  [eax+TCorner.type],edx
           inc  [esi+TConfiguration.ConvexCornersCount]
     .quit:pop edi
ret

align 16
;esi - TConfiguration
;ecx - TCandidate
;Placing rectangle in PackedRect list and rebuilding other lists (ConcaveCrorners, ConvexCorners, CandidateRects)
PlaceRect:
  pushad
  mov    edi,[esi+TConfiguration.PackedRects]
  mov    ebx,[esi+TConfiguration.PackedRectsCount]
  mov    eax,[ecx+TCandidate.Right]
  mov    edx,[ecx+TCandidate.Top]
  shl    ebx,5
  sub    eax,[ecx+TCandidate.Left]
  sub    edx,[ecx+TCandidate.Bottom]
  movdqa xmm0,[ecx]
  movdqa xmm1,[ecx+16]
  mul    edx
  movdqa [edi+ebx],xmm0
  movdqa [edi+ebx+16],xmm1
  add    dword[esi+TConfiguration.Area],eax
  adc    dword[esi+TConfiguration.Area+4],edx
  inc    [esi+TConfiguration.PackedRectsCount]
  movzx  eax,[ecx+TCandidate.UnpackedRectIndex]
  mov    edx,[esi+TConfiguration.UnpackedRectsCount]
  mov    ebp,[esi+TConfiguration.UnpackedRects]
  shl    eax,5
  shl    edx,4
  sub    [esi+TConfiguration.UnpackedRectsCount],2
  movdqa xmm0,[ebp+edx-32]
  movdqa xmm1,[ebp+edx-16]
  movdqa [ebp+eax],xmm0
  movdqa [ebp+eax+16],xmm1
  mov    eax,[esi+TConfiguration.PackHeight]
  mov    [esi+TConfiguration.ConcaveCornersCount],0
  cmp    eax,[ecx+TCandidate.Top]
  mov    [esi+TConfiguration.ConvexCornersCount],0
  cmovc  eax,[ecx+TCandidate.Top]
  mov    [esi+TConfiguration.PackHeight],eax
  @@:movq   xmm0,qword[edi+ebx+TCandidate.Left]        ;(Left,Bottom)
     call   AddCorner
     pshufd xmm0,dqword[edi+ebx+TCandidate.Left],0110b ;(Right,Bottom)
     call   AddCorner
     pshufd xmm0,dqword[edi+ebx+TCandidate.Left],1100b ;(Left,Top)
     call   AddCorner
     movq   xmm0,qword[edi+ebx+TCandidate.Right]       ;(Right,Top)
     call   AddCorner
     sub    ebx,sizeof.TCandidate
  jns @b
  call   FindCandidates
  popad
ret

align 16
Search: ;(ThreadIndex: dword);stdcall;
ThreadIndex equ esp+4
  imul      esi,dword[ThreadIndex],sizeof.TConfiguration
  add       esi,Cxx
  .TrhreadLoop:
    mov       eax,[ThreadIndex]
    invoke    WaitForSingleObject,[InThreadEvents+eax*4],-1 ;INFINITE

    pxor      xmm0,xmm0
    movq      [esi+TConfiguration.MaxArea],xmm0
    mov       [esi+TConfiguration.BestHeight],0
    mov       eax,[C.CandidateRectsCount]
    cdq
    div       [ThreadCount]
    mov       ebx,eax
    imul      eax,[ThreadIndex]
    cmp       [ThreadIndex],edx
    cmovc     edx,[ThreadIndex]
    adc       ebx,0             ;QuantSize
    lea       edi,[eax+edx]     ;QuantStart

    cmp edi,[C.CandidateRectsCount]
    jnc .continue
      add       ebx,edi
      cmp       ebx,[C.CandidateRectsCount]
      cmova     ebx,[C.CandidateRectsCount]
      sub       ebx,edi
      shl       ebx,5
      shl       edi,5
      add       edi,[C.CandidateRects]

      mov       eax,[C.PackedRectsCount]                     ;eax may contains 0, but it`s ok
      mov       edx,[C.PackedRects]
      mov       ecx,[esi+TConfiguration.PackedRects]
      shl       eax,5
      @@:sub    eax,sizeof.TCandidate
          movdqa xmm0,[edx+eax]
          movdqa xmm1,[edx+eax+16]
          movdqa [ecx+eax],xmm0
          movdqa [ecx+eax+16],xmm1
      jnle @b

      .CandidateTry:
        cmp       [BreakPacking],0
        jne       .quit

        movq      xmm2,[C.Area]
        mov       ecx,[C.PackedRectsCount]                   ;ecx may contains 0, but it`s ok
        mov       [esi+TConfiguration.PackedRectsCount],ecx
        mov       eax,[C.PackedRects]
        mov       edx,[esi+TConfiguration.PackedRects]
        shl       ecx,5
        movdqa    xmm0,[eax+ecx-32]
        movdqa    xmm1,[eax+ecx-16]
        movdqa    [edx+ecx-32],xmm0
        movdqa    [edx+ecx-16],xmm1
        mov       eax,[C.UnpackedRectsCount]
        mov       edx,[C.PackHeight]
        movq      [esi+TConfiguration.Area],xmm2
        mov       [esi+TConfiguration.UnpackedRectsCount],eax
        mov       [esi+TConfiguration.PackHeight],edx
        mov       edx,[C.UnpackedRects]
        mov       ecx,[esi+TConfiguration.UnpackedRects]

        shl       eax,4
        @@:sub    eax,16
           movdqa xmm0,[edx+eax]
           movdqa [ecx+eax],xmm0
        jne @b

        lea  ecx,[edi+ebx-sizeof.TCandidate]
        push ebx
        push edi
        jmp .start
        .BuildConfiguration:
            movss  xmm3,[maxrate]
            pxor   xmm1,xmm1
            imul   ebx,[esi+TConfiguration.CandidateRectsCount],sizeof.TCandidate
            .FindBest:
              mov    edi,[esi+TConfiguration.CandidateRects]
              mov    ecx,4
              pshufd xmm0,dqword[edi+ebx+TCandidate.Left-sizeof.TCandidate],11011000b
              imul   edx,[esi+TConfiguration.ConvexCornersCount],sizeof.TCorner
              test   edx,edx
              je @f
                mov    edi,[esi+TConfiguration.ConvexCorners]
                .convex:
                  pshufd   xmm2,dqword[edi+edx-sizeof.TCorner+TCorner.x],01010000b
                  pcmpeqd  xmm2,xmm0
                  pcmpeqq  xmm2,xmm1
                  pmovmskb eax,xmm2
                  cmp      eax,1
                  sbb      ecx,0
                  sub      edx,sizeof.TCorner
                jne .convex
              @@:
              imul   edx,[esi+TConfiguration.ConcaveCornersCount],sizeof.TCorner
              mov    edi,[esi+TConfiguration.ConcaveCorners]
              .concave:
                pshufd   xmm2,dqword[edi+edx-sizeof.TCorner+TCorner.x],01010000b
                pcmpeqd  xmm2,xmm0
                pcmpeqq  xmm2,xmm1
                pmovmskb eax,xmm2
                cmp      eax,1
                sbb      ecx,0
                sub      edx,sizeof.TCorner
              jne .concave
              mov      edi,[esi+TConfiguration.CandidateRects]
              shl      ecx,cl
              imul     ecx,[edi+ebx-sizeof.TCandidate+TCandidate.Top]
              cvtsi2ss xmm0,ecx
              cvtsi2ss xmm2,[edi+ebx-sizeof.TCandidate+TCandidate.HalfPerimeter]
              divss    xmm0,xmm2
              comiss   xmm0,xmm3
              minss    xmm3,xmm0
              cmovc    ebp,ebx
              sub ebx,sizeof.TCandidate
            jne .FindBest
            lea    ecx,[ebp-32]
            add    ecx,[esi+TConfiguration.CandidateRects]
        .start:
            call   PlaceRect
            cmp    [esi+TConfiguration.CandidateRectsCount],0
        ja .BuildConfiguration
        pop  edi
        pop  ebx

        movq      xmm0,qword[esi+TConfiguration.PackHeight]
        movdqu    xmm1,dqword[esi+TConfiguration.MaxArea]
        punpckldq xmm0,xmm0
        psrlq     xmm0,32
        paddq     xmm0,xmm1
        movhlps   xmm1,xmm0
        pcmpgtq   xmm1,xmm0
        movd      eax,xmm1
        test      eax,eax
        je .below
          movq xmm0,[esi+TConfiguration.Area]
          movq [esi+TConfiguration.MaxArea],xmm0
          mov  eax,[esi+TConfiguration.PackHeight]
          mov  [esi+TConfiguration.BestHeight],eax
          mov  eax,[esi+TConfiguration.PackedRectsCount]
          mov  [esi+TConfiguration.BestPackCount],eax
          mov  edx,[esi+TConfiguration.PackedRects]
          mov  ecx,[esi+TConfiguration.BestPack]
          shl  eax,5
          @@:movdqa xmm0,[edx+eax-32]
             movdqa xmm1,[edx+eax-16]
             movdqa [ecx+eax-32],xmm0
             movdqa [ecx+eax-16],xmm1
             sub    eax,32
          jne @b
        .below:

        sub ebx,sizeof.TCandidate
      jne .CandidateTry
    .continue:
    mov    eax,[ThreadIndex]
    invoke SetEvent,[OutThreadEvents+eax*4]
    cmp    [BreakPacking],0
  je .TrhreadLoop
  .quit:
  mov    ebx,[ThreadIndex]
  invoke CloseHandle,[InThreadEvents+ebx*4]
  invoke SetEvent,[OutThreadEvents+ebx*4]
  invoke CloseHandle,[OutThreadEvents+ebx*4]
ret 4

align 16
;Main packing thread
Pack:
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
    lea    esi,[C+eax]
    mov    [esi+TConfiguration.PackedRectsCount],0
    mov    [esi+TConfiguration.BestPackCount],0
    mov    [esi+TConfiguration.PackHeight],0
    mov    dword[esi+TConfiguration.Area],0
    mov    dword[esi+TConfiguration.Area+4],0
    mov    eax,[ShapesCount]
    mov    edx,eax
    imul   edx,eax
    shl    edx,8                                   ;ShapesCount*ShapesCount*8*sizeof.TCandidate
    mov    [esi+TConfiguration.PackedRects],edx
    shl    eax,1
    mov    [esi+TConfiguration.UnpackedRectsCount],eax
    shl    eax,4
    add    edx,eax                                 ;+ShapesCount*sizeof.TCandidate
    mov    [esi+TConfiguration.BestPack],edx
    add    edx,eax                                 ;+ShapesCount*sizeof.TCandidate
    mov    [esi+TConfiguration.ConcaveCorners],edx
    lea    edx,[edx+eax*2]                         ;+ShapesCount*sizeof.TCorner*4
    mov    [esi+TConfiguration.ConvexCorners],edx
    lea    edx,[edx+eax*2]                         ;+ShapesCount*sizeof.TCorner*4
    mov    [esi+TConfiguration.UnpackedRects],edx
    add    edx,eax                                 ;+ShapesCount*sizeof.TSize*2
    invoke VirtualAlloc,0,edx,MEM_COMMIT,PAGE_READWRITE
    mov    [esi+TConfiguration.CandidateRects],eax
    add    [esi+TConfiguration.PackedRects],eax
    add    [esi+TConfiguration.BestPack],eax
    add    [esi+TConfiguration.ConcaveCorners],eax
    add    [esi+TConfiguration.ConvexCorners],eax
    add    [esi+TConfiguration.UnpackedRects],eax
    dec    ebx
    js @f
      invoke CreateEventW,0,0,0,0
      mov    [InThreadEvents+ebx*4],eax
      invoke CreateEventW,0,0,0,0
      mov    [OutThreadEvents+ebx*4],eax
      lea    eax,[ThreadIds+ebx*4]
      invoke CreateThread,0,0,Search,ebx,0,eax
  jmp .InitConfigurations
  @@:

;;;;;;;;;;;;;;;    Getting shapes from CorelDraw    ;;;;;;;;;;;;;;;
  cominvk CorelApp,Set_Optimization,1
  mov     ebx,[ShapesCount]
  mov     edi,ebx
  shl     edi,5
  add     edi,[C.UnpackedRects]
  @@:sub      edi,sizeof.TSize*2
     push     0                                  ; reserve memory for IVGShape, so esp would point to Shapes array
     cominvk  Selection,Get_Item,VT_I4,0,ebx,0,esp
     mov      ebp,[esp]
     comcall  ebp,IVGShape,Set_RotationAngle,0,0 ;2 times because double
     comcall  ebp,IVGShape,Get_SizeWidth,buf
     comcall  ebp,IVGShape,Get_SizeHeight,buf+8
     cvtsi2sd xmm0,[Span]
     cvtpi2pd xmm1,qword[SCALE_FACTOR]
     movddup  xmm0,xmm0
     addpd    xmm0,dqword[buf]
     mulpd    xmm0,xmm1
     cvtpd2dq xmm0,xmm0
     movq     qword[edi+TSize.Width],xmm0
     mov      [edi+TSize.Shape],ebp
     pshufd   xmm0,xmm0,1
     movq     qword[edi+sizeof.TSize+TSize.Width],xmm0
     mov      [edi+sizeof.TSize+TSize.Shape],ebp
     mov      [edi+sizeof.TSize+TSize.Angle],90
     dec      ebx
  jne @b
  cominvk Selection,Release

;;;;;;;;;;;;;;;    Random shapes shuffle (if selected)    ;;;;;;;;;;;;;;;
  invoke SendDlgItemMessageW,[DialogWindow],4,BM_GETSTATE,0,0
  test   eax,BST_CHECKED
  je @f
    mov ecx,[ShapesCount]
    mov ebx,[C.UnpackedRects]
    shl ecx,5
    .Randomize:
      rdtsc
      mul    edx            ;I don`t want use crc32 or rdrand because old cpu compatibility
      xor    edx,edx
      div    [ShapesCount]
      shl    edx,5
      sub    ecx,sizeof.TSize*2
      movdqa xmm0,[ebx+edx]
      movdqa xmm1,[ebx+edx+sizeof.TSize]
      movdqa xmm2,[ebx+ecx]
      movdqa xmm3,[ebx+ecx+sizeof.TSize]
      movdqa [ebx+ecx],xmm0
      movdqa [ebx+ecx+sizeof.TSize],xmm1
      movdqa [ebx+edx],xmm2
      movdqa [ebx+edx+sizeof.TSize],xmm3
    jne .Randomize
  @@:

;;;;;;;;;;;;;;;    Rectangle packing routine    ;;;;;;;;;;;;;;;
  mov    eax,[C.ConcaveCorners]
  mov    [eax+TCorner.x],0
  mov    [eax+TCorner.y],0
  mov    [eax+TCorner.type],1 ;Bottom left corner
  mov    [C.ConcaveCornersCount],1
  mov    esi,C
  call   FindCandidates
  mov    dword[C.MaxArea],0
  mov    dword[C.MaxArea+4],0
  mov    [C.BestHeight],0
  jmp .StartPacking
  .RectPack:
    mov ebx,[ThreadCount]
    @@:invoke SetEvent,[InThreadEvents+ebx*4-4]
       dec    ebx
    jne @b
    cmp    [BreakPacking],0
    jne .quit
    invoke WaitForMultipleObjects,[ThreadCount],OutThreadEvents,1,-1

    imul   edi,[ThreadCount],sizeof.TConfiguration
    xor    ebp,ebp
    .FindBest:
       mov eax,dword[C.MaxArea]
       mov edx,dword[C.MaxArea+4]
       mov ebx,dword[C.MaxArea+edi]
       mov ecx,dword[C.MaxArea+edi+4]
       add eax,[C.BestHeight+edi]
       adc edx,0
       add ebx,[C.BestHeight]
       adc ecx,0
       cmp edx,ecx
       ja @f
         cmp eax,ebx
         jae @f
           mov  ebp,edi
           movq xmm0,[C.MaxArea+edi]
           mov  eax,[C.BestHeight+edi]
           movq [C.MaxArea],xmm0
           mov  [C.BestHeight],eax
       @@:
       sub edi,sizeof.TConfiguration
    jne .FindBest

    test ebp,ebp
    je .NotFound
      cominvk CorelApp,Set_Optimization,1
      mov     ebx,[ShapesCount]
      mov     edi,esp
      @@:sub      esp,16
         mov      eax,[Width]
         add      eax,UNPACKED_OFFSET
         xorpd    xmm0,xmm0
         cvtsi2sd xmm0,eax
         mulsd    xmm0,[RCP_SCALE_FACTOR]
         addpd    xmm0,[Origin]
         movupd   [esp],xmm0
         comcall  [edi+ebx*4-4],IVGShape,SetPosition
         dec      ebx
      jne @b
      mov ebx,[C.BestPackCount+ebp]
      mov ebp,[C.BestPack+ebp]
      mov edi,[C.BestPack]
      mov [C.BestPackCount],ebx
      shl ebx,5
      @@:movdqa   xmm0,[ebp+ebx-32]
         movdqa   xmm1,[ebp+ebx-16]
         movdqa   [edi+ebx-32],xmm0
         movdqa   [edi+ebx-16],xmm1
         movzx    eax,[ebp+ebx-sizeof.TCandidate+TCandidate.Angle]
         cvtsi2sd xmm0,eax
         sub      esp,8
         movsd    [esp],xmm0
         comcall  [ebp+ebx-sizeof.TCandidate+TCandidate.Shape],IVGShape,Set_RotationAngle
         cvtpi2pd xmm0,qword[ebp+ebx-sizeof.TCandidate+TCandidate.Left]
         sub      esp,16
         mulpd    xmm0,[RCP_SCALE_FACTOR]
         addpd    xmm0,[Origin]
         movupd   [esp],xmm0
         comcall  [ebp+ebx-sizeof.TCandidate+TCandidate.Shape],IVGShape,SetPosition
         sub      ebx,sizeof.TCandidate
      jne @b
      cominvk  CorelApp,Set_Optimization,0
      cmp      [BreakPacking],0
      jne @f
        cvtsi2sd xmm0,[C.BestHeight]
        mulsd    xmm0,[RCP_SCALE_FACTOR]
        cvtsd2si eax,xmm0
        cinvoke  wsprintfW,buf,fmt,[C.BestPackCount],[ShapesCount],eax
        invoke   SendMessageW,[DialogWindow],COREL_REFRESH,0,0
        invoke   SendMessageW,[DialogWindow],WM_SETTEXT,0,buf
      @@:
    .NotFound:

    invoke  PostMessageW,[ProgressBar],PBM_STEPIT,0,0
    mov     ecx,[C.PackedRectsCount]
    shl     ecx,5
    add     ecx,[C.BestPack]
    mov     esi,C
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
    mov      ebp,[C.BestPack]
    shl      ebx,5
    .DrawFrames:
      cvtpi2pd xmm0,qword[ebp+ebx-sizeof.TCandidate+TCandidate.Left]
      cvtpi2pd xmm1,qword[ebp+ebx-sizeof.TCandidate+TCandidate.Right]
      mulpd    xmm0,[RCP_SCALE_FACTOR]
      mulpd    xmm1,[RCP_SCALE_FACTOR]
      addpd    xmm0,[Origin]
      addpd    xmm1,[Origin]
      push     Shape
      sub      esp,48
      xorpd    xmm2,xmm2
      movupd   [esp],xmm0
      movupd   [esp+16],xmm1
      movupd   [esp+32],xmm2
      cominvk  Layer,CreateRectangle
      cominvk  Shape,Get_Outline,Outline
      cominvk  Outline,Set_Width,0xDBF487FD,0x3FB381D7 ;0.0762 double-precision (ultra thin outline constant)
      cominvk  Outline,Release
      cominvk  Shape,Release
      sub      ebx,sizeof.TCandidate
    jne .DrawFrames
    cominvk  Layer,Release
  @@:

;;;;;;;;;;;;;;;    Free resources    ;;;;;;;;;;;;;;;
  mov   ebx,[ShapesCount]
  @@:pop     eax
     comcall eax,IVGShape,Release
     dec     ebx
  jne @b
  cominvk CorelApp,Set_Optimization,0
  imul    ebx,[ThreadCount],sizeof.TConfiguration
  @@:invoke VirtualFree,[C+ebx+TConfiguration.CandidateRects],0,MEM_RELEASE
     sub    ebx,sizeof.TConfiguration
  jns @b
  invoke  PostMessageW,[DialogWindow],WM_CLOSE,0,0
ret 4

align 16
DialogFunc: ;(wnd,msg,wParam,lParam: dword):dword;stdcall;
  cmp dword[esp+8],COREL_REFRESH
  je .COREL_REFRESH
  cmp dword[esp+8],WM_COMMAND
  je .WM_COMMAND
  cmp dword[esp+8],WM_INITDIALOG
  je .WM_INITDIALOG
  cmp dword[esp+8],WM_CLOSE
  je .WM_CLOSE
    xor eax,eax
  ret 16
  .COREL_REFRESH:cominvk CorelApp,Refresh
                 invoke  SendMessageW,[CorelWndHandle],WM_MBUTTONDOWN,0,0 ;Redraw hack for old Corel versions
                 invoke  SendMessageW,[CorelWndHandle],WM_MBUTTONUP,0,0
                 ret 16
     .WM_COMMAND:cmp dword[esp+12],(BN_CLICKED shl 16)+5
                 je  .DrawFramesChange
                 cmp dword[esp+12],(BN_CLICKED shl 16)+6
                 jne @f
                   mov    [BreakPacking],0
                   invoke EnableWindow,dword[esp+20],0
                   invoke SendDlgItemMessageW,dword[esp+20],1,WM_GETTEXT,bufsize/2,buf
                   call   BufStr2IntW
                   mov    [Width],eax
                   invoke SendDlgItemMessageW,dword[esp+20],2,WM_GETTEXT,bufsize/2,buf
                   call   BufStr2IntW
                   mov    [Height],eax
                   invoke SendDlgItemMessageW,dword[esp+20],3,WM_GETTEXT,bufsize/2,buf
                   call   BufStr2IntW
                   mov    [Span],eax
                   invoke CreateThread,0,4096*1024,Pack,0,0,0
                   mov    [PackTreadHandle],eax
                   .DrawFramesChange:
                   invoke SendDlgItemMessageW,dword[esp+20],5,BM_GETSTATE,0,0
                   and    eax,BST_CHECKED
                   mov    [DrawFrames],eax
                 @@:
                 ret 16
  .WM_INITDIALOG:mov    eax,[esp+4]
                 mov    [DrawFrames],0
                 mov    [DialogWindow],eax
                 invoke SendDlgItemMessageW,eax,1,EM_SETLIMITTEXT,5,0
                 invoke SendDlgItemMessageW,dword[esp+20],2,EM_SETLIMITTEXT,5,0
                 invoke SendDlgItemMessageW,dword[esp+20],3,EM_SETLIMITTEXT,3,0
                 invoke GetDlgItem,dword[esp+8],7
                 mov    [ProgressBar],eax
                 invoke PostMessageW,eax,PBM_SETSTEP,1,0
                 ret 16
       .WM_CLOSE:lock inc [BreakPacking]
                 invoke WaitForSingleObject,[PackTreadHandle],-1;INFINITE
                 invoke EndDialog,dword[esp+8],0
                 ret 16

QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov eax,[esp+12]
  mov dword[eax],IPlugin
  xor eax,eax
ret 12
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret 4
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 8
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 12
GetIDsOfNames:    ;(const self:IVGAppPlugin; const IID: TGUID; Names: Pointer;NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 24

Invoke:           ;(const self:IVGAppPlugin; DispID: Integer; const IID: TGUID; LocaleID: Integer;Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  mov eax,[esp+8]
  cmp eax,SelectionChange
  je .SelectionChange
  cmp eax,OnPluginCommand
  je .OnPluginCommand
  cmp eax,OnUpdatePluginCommand
  je .OnUpdatePluginCommand
  xor eax,eax
  ret 36
        .SelectionChange:cominvk CorelApp,Get_ActiveSelectionRange,Selection
                         test    eax,eax
                         jne @f
                           cominvk Selection,Get_Count,Enabled
                           cominvk Selection,Release
                         @@:
                         xor     eax,eax
                         ret 36
        .OnPluginCommand:mov    eax,[esp+24]
                         mov    eax,[eax+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,dword[eax+VARIANT.data],strDirectEnpack
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
                         ret 36
  .OnUpdatePluginCommand:xchg   ebx,[esp+24]
                         mov    ebx,[ebx+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,dword[ebx+sizeof.VARIANT*2+VARIANT.data],strDirectEnpack
                         test   eax,eax
                         jne    @f
                           mov eax,dword[ebx+sizeof.VARIANT*1+VARIANT.data]
                           mov edx,[Enabled]
                           mov [eax],dx
                         @@:
                         mov    ebx,[esp+24]
                         xor    eax,eax
                         ret 36

OnLoad:           ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  xchg    ebx,[esp+8]
  mov     [CorelApp],ebx
  comcall ebx,IVGApplication,AddRef
  comcall ebx,IVGApplication,Get_VersionMinor,CorelVersion
  comcall ebx,IVGApplication,Get_VersionMajor,CorelVersion+1
  mov     ebx,[esp+8]
ret 8

StartSession:     ;(const self:IVGAppPlugin):LongInt;stdcall;
  push    ebx
  mov     eax,1
  cpuid
  test    ecx,1 shl 19 ;SSE 4.1
  je .CPUNotSupported
    mov     ebx,[CorelApp]
    comcall ebx,IVGApplication,AddPluginCommand,strDirectEnpack,strButtonCaption,strButtonCaption,buf
    comcall ebx,IVGApplication,AdviseEvents,IPlugin,EventsCookie
    comcall ebx,IVGApplication,Get_CommandBars,CommandBars
    cominvk CommandBars,Get_Item,VT_BSTR,0,strStandard,0,CommandBar
    cominvk CommandBar,Get_Controls,Controls
    cominvk Controls,Get_Count,buf
    mov     ebx,dword[buf]
    @@:cominvk Controls,Get_Item,ebx,Control
       push    eax
       cominvk Control,Get_Caption,esp
       invoke  lstrcmpW,strButtonCaption
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
    lea     eax,[buf+eax-sizeof.strDirectEnpack-2]
    invoke  lstrcpyW,eax,strDirectEnpack
    cmp     [CorelVersion],1104h ;17.4
    push    buf+4
    jb .legacy
      mov     dword[eax+sizeof.strDirectEnpack-2],69002Eh ;'.i'
      mov     dword[eax+sizeof.strDirectEnpack+2],6F0063h ;'co'
      invoke  CreateFileW,buf+4,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0
      push    eax
      invoke  WriteFile,eax,ICOData,sizeof.ICOData,Span,0
      invoke  CloseHandle
      cominvk Control,SetIcon2
      jmp @f
    .legacy:
      mov     dword[eax+sizeof.strDirectEnpack-2],62002Eh ;'.b'
      mov     dword[eax+sizeof.strDirectEnpack+2],70006Dh ;'mp'
      invoke  CreateFileW,buf+4,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0
      push    eax
      invoke  WriteFile,eax,BMPData,sizeof.BMPData,Span,0
      invoke  CloseHandle
      cominvk Control,SetCustomIcon
    @@:
    cominvk Control,Release
    cominvk Controls,Release
    cominvk CommandBar,Release
    cominvk CommandBars,Release
    pop     ebx
    xor     eax,eax
    ret 4
  .CPUNotSupported:
  invoke MessageBoxW,[CorelWndHandle],errCPUNotSupported,strDirectEnpack,MB_TASKMODAL
  pop    ebx
  mov    eax,E_FAIL
ret 4

StopSession:      ;(const self:IVGAppPlugin):LongInt;stdcall;
  cominvk CorelApp,UnadviseEvents,[EventsCookie]
  xor     eax,eax
ret 4

OnUnload:         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CorelApp,Release
  xor     eax,eax
ret 4

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
InThreadEvents  rd MAX_THREADS
OutThreadEvents rd MAX_THREADS
ThreadIds       rd MAX_THREADS
C               TConfiguration
Cxx             rb MAX_THREADS*sizeof.TConfiguration
CorelWndHandle  rd 1
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
ProgressBar     rd 1
BreakPacking    rd 1
PackTreadHandle rd 1
ShapesCount     rd 1
ThreadCount     rd 1
DialogWindow    rd 1
EventsCookie    rd 1
Enabled         rd 1
CorelVersion    rd 1
SysInfo         SYSTEM_INFO