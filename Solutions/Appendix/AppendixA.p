program MirandaStuff;

{ Examples from Appendix A, showing the equivalents in an
  imperative language of tuples and list-handling techniques.

  (c) Simon Thompson 1995
}

type person = record 
  name  : string;
  phone : string;
  age   : integer
end;

type value = integer;

type list = ^node;
     node = record
       head : value;
       tail : list
     end;

function cons(b:value;y:list):list;
  var l:list;
  begin
    new(l);
    l^.head := b;
    l^.tail := y;
    cons := l
  end;

function sumList(l:list):integer;
  begin
    if l=nil 
      then sumList := 0
      else sumList := l^.head + sumList(l^.tail)
  end;

function double(l:list):list;
  begin
    if l=nil 
      then double := nil
      else double := cons( 2*l^.head , double(l^.tail) )
  end;

function hd(l:list):value;
  begin                  
    hd := l^.head    
  end;                 

function tl(l:list):list;
  begin
    tl := l^.tail
  end; 

function doubleNew(l:list):list;
  begin
    if l=nil 
      then doubleNew := nil
      else doubleNew := cons( 2*hd(l) , doubleNew( tl(l) ) )
  end;

begin
end.
