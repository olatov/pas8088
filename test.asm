start:
  mov ax, 0x0060
  mov ds, ax
  mov ss, ax
  mov ax, 0xb800
  mov es, ax

  ;lds si,[0x7c]
  ;mov dx, ds
  
  ;mov al, 65
  ;mov [es:0], al

  mov al, 66
  mov [es:162], al

  ;mov ax, 20
  ;mov bx, 3
  ;div bl
  
end:
  hlt
