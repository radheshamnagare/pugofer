
comp:(Int)->[[Int]]
comp.(-1)=[]
comp.(x)=find_conse.(reverse.(binary.(x))::comp.(x-1))
binary.(x)
     |x>0 =remd.(x)::binary.(x/2)
     |otherwise =[]
remd.(x)
     | x==1 =1
     | x>=2 =(x-(x/2)*2)

find_conse:[[Int]]->[[Int]]
find_conse.([])=[]
find_conse.(x::xs)
  |f4.(x)==False =x::find_conse.(xs)
  |otherwise =find_conse.(xs)
f4:[Int]->Bool
f4.([])=False
f4.(x::[])=False
f4.(x::xs)
 | ((head.(x::xs)==1)&&(head.(xs)==1)) =True
 | otherwise =f4.(xs)


add_zero:([[Int]])->[[Int]]
add_zero.(x::xs)=add_zero1.(x::xs,length.(x))

add_zero1:([[Int]],Int)->[[Int]]
add_zero1.([],l)=[]
add_zero1.(x::xs,l)=add_z.(x,l)::add_zero1.(xs,l)

add_z:([Int],Int)->[Int]
add_z.(l,x)
 |length.(l)==x =l
 |otherwise =add_z.(0::l,x)

all_conse.(x)=add_zero.(comp.(x))
