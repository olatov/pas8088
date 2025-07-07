start:
  mov ax, 0x0060
  mov ds, ax
  mov es, ax
  mov ss, ax

  mov ax, 0
  mov cx, 10
repeat:
  inc ax
  loop repeat
  mov bx, foo
  mov cx, [bx]
  mov dx, cx
end:
  hlt

foo: dw 0x1234

