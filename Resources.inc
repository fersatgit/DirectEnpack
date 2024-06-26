macro DLGTEMPLATEEX helpID,exStyle,style,cDlgItems,x,y,cx,cy,_menu,title,pointsize,weight,italic,charset,typeface{
  dd 0xFFFF0001,helpID,exStyle,style
  dw cDlgItems,x,y,cx,cy,_menu,0
  du title,0
  dw pointsize,weight
  db italic,charset
  du typeface,0
}

macro DLGITEMTEMPLATEEX helpID,exStyle,style,x,y,cx,cy,id,class,title{
  align 4
  dd helpID,exStyle,style
  dw x,y,cx,cy
  dd id
  if class eq 'BUTTON'
    dw 0FFFFh,80h
  else if class eq 'EDIT'
    dw 0FFFFh,81h
  else if class eq 'STATIC'
    dw 0FFFFh,82h
  else if class eq 'LISTBOX'
    dw 0FFFFh,83h
  else if class eq 'SCROLLBAR'
    dw 0FFFFh,84h
  else if class eq 'COMBOBOX'
    dw 0FFFFh,85h
  else
    du class,0
  end if
  du title,0
  dw 0
}

align 16
mult:                   dd 0, 0,1,1,\
                          -1, 0,0,1,\
                           0, 0,0,0,\
                           0,-1,1,0,\
                           0, 0,0,0,\
                           0, 0,0,0,\
                           0, 0,0,0,\
                          -1,-1,0,0
corners:                dd +1,+1,\
                           -1,+1,\
                           +1,-1,\
                           -1,-1
dbl_05:                 dq 0.5,0.5
RCP_SCALE_FACTOR:       dq 0.01,0.01
SCALE_FACTOR:           dd 100,100
UNPACKED_OFFSET=10000
rcp_sizeof_unpackedRect dd 0.03125 ;0.5/sizeof_unpackedRect;
maxrate                 dd 2147483647.0
MainDlg:                DLGTEMPLATEEX 0,0,DS_SETFONT+DS_MODALFRAME+DS_CENTER+WS_POPUP+WS_CAPTION+WS_SYSMENU,8,0,0,132,60,0,'',9,FW_NORMAL,0,0,'Tahoma'
                        ;encoding macro don`t work here. I don`t know why. So I define unicode-string by hand.
                        EDIT_CAPTIONS equ 0428h,0438h,0440h,0438h,043Dh,0430h,000Dh,000Ah,000Dh,000Ah,0412h,044Bh,0441h,043Eh,0442h,0430h,000Dh,000Ah,000Dh,000Ah,041Eh,0442h,0441h,0442h,0443h,043Fh;'Ширина',13,10,13,10,'Высота',13,10,13,10,'Отступ'
                        DLGITEMTEMPLATEEX 0,0,SS_LEFT+WS_CHILD+WS_VISIBLE                               ,2 ,2 ,30 ,40,0,'STATIC',EDIT_CAPTIONS
                        DLGITEMTEMPLATEEX 0,0,ES_LEFT+ES_NUMBER+WS_CHILD+WS_VISIBLE+WS_BORDER+WS_TABSTOP,36,1 ,25 ,12,1,'EDIT','1200'
                        DLGITEMTEMPLATEEX 0,0,ES_LEFT+ES_NUMBER+WS_CHILD+WS_VISIBLE+WS_BORDER+WS_TABSTOP,36,16,25 ,12,2,'EDIT','2400'
                        DLGITEMTEMPLATEEX 0,0,ES_LEFT+ES_NUMBER+WS_CHILD+WS_VISIBLE+WS_BORDER+WS_TABSTOP,36,32,25 ,12,3,'EDIT','0'
                        DLGITEMTEMPLATEEX 0,0,BS_AUTOCHECKBOX+WS_CHILD+WS_VISIBLE+WS_TABSTOP            ,67,1 ,62 ,12,4,'BUTTON','Перемешать'
                        DLGITEMTEMPLATEEX 0,0,BS_AUTOCHECKBOX+WS_CHILD+WS_VISIBLE+WS_TABSTOP            ,67,16,62 ,12,5,'BUTTON','Добавить рамку'
                        DLGITEMTEMPLATEEX 0,0,BS_DEFPUSHBUTTON+WS_CHILD+WS_VISIBLE+WS_TABSTOP           ,70,33,50 ,11,6,'BUTTON','&OK'
                        DLGITEMTEMPLATEEX 0,0,WS_VISIBLE+WS_CHILD                                       ,2 ,48,128,10,7,'msctls_progress32',''
                        dd sizeof.strDirectEnpack-2
strDirectEnpack         du 'DirectEnpack',0
sizeof.strDirectEnpack=$-strDirectEnpack
                        dd 16
strStandard             du 'Standard',0
                        dd 42
strButtonCaption        du 'Прямоугольный раскрой',0
errCPUNotSupported      du 'Процессор не поддерживается. Требуется SSE 4.1.',0
fmt                     du '%u/%u, высота %u мм',0
ICOData                 file 'icon.ico'
sizeof.ICOData=$-ICOData
BMPData                 file 'icon.bmp'
sizeof.BMPData=$-BMPData

data import
  library kernel,'KERNEL32.DLL',\
          user,'USER32'

  import kernel,\
         CloseHandle,'CloseHandle',\
         CreateEventW,'CreateEventW',\
         CreateFileW,'CreateFileW',\
         CreateThread,'CreateThread',\
         ExitThread,'ExitThread',\
         GetSystemInfo,'GetSystemInfo',\
         GetTempPathW,'GetTempPathW',\
         lstrcmpW,'lstrcmpW',\
         lstrcpyW,'lstrcpyW',\
         SetEvent,'SetEvent',\
         VirtualAlloc,'VirtualAlloc',\
         VirtualFree,'VirtualFree',\
         WaitForMultipleObjects,'WaitForMultipleObjects',\
         WaitForSingleObject,'WaitForSingleObject',\
         WriteFile,'WriteFile'

  import user,\
         DialogBoxIndirectParamW,'DialogBoxIndirectParamW',\
         EnableWindow,'EnableWindow',\
         EndDialog,'EndDialog',\
         GetDlgItem,'GetDlgItem',\
         MessageBoxW,'MessageBoxW',\
         PostMessageW,'PostMessageW',\
         SendMessageW,'SendMessageW',\
         SendDlgItemMessageW,'SendDlgItemMessageW',\
         wsprintfW,'wsprintfW'

end data

data export
    export 0,AttachPlugin,'AttachPlugin'
end data

data fixups
end data