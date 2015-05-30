file = tempname()
s = open(file, "w") do f
    write(f, "Hello World\n")
end
t = "Hello World".data
@test Mmap.Array(UInt8, (11,1,1), file) == reshape(t,(11,1,1))
gc()
@test Mmap.Array(UInt8, (1,11,1), file) == reshape(t,(1,11,1))
gc()
@test Mmap.Array(UInt8, (1,1,11), file) == reshape(t,(1,1,11))
gc()
@test_throws ArgumentError Mmap.Array(UInt8, (11,0,1), file) # 0-dimension results in len=0
@test Mmap.Array(UInt8, (11,), file) == t
gc()
@test Mmap.Array(UInt8, (1,11), file) == t'
gc()
@test_throws ArgumentError Mmap.Array(UInt8, (0,12), file)
m = Mmap.Array(UInt8, (1,2,1),  file)
@test m == reshape("He".data,(1,2,1))
m=nothing; gc()

s = open(f->f,file,"w")
@test_throws ArgumentError Mmap.Array(file) # requested len=0 on empty file
@test_throws ArgumentError Mmap.Array(file,0)
m = Mmap.Array(file,12)
m[:] = "Hello World\n".data
Mmap.sync!(m)
m=nothing; gc()
@test open(readall,file) == "Hello World\n"

s = open(file, "r")
close(s)
@test_throws Base.UVError Mmap.Array(s) # closed IOStream
@test_throws ArgumentError Mmap.Array(s,12,0) # closed IOStream
@test_throws SystemError Mmap.Array("")

# negative length
@test_throws ArgumentError Mmap.Array(file, -1)
# negative offset
@test_throws ArgumentError Mmap.Array(file, 1, -1)

for i = 0x01:0x0c
    @test length(Mmap.Array(file, i)) == Int(i)
end
gc()

sz = filesize(file)
m = Mmap.Array(file, sz+1)
@test length(m) == sz+1 # test growing
@test m[end] == 0x00
m=nothing; gc()
sz = filesize(file)
m = Mmap.Array(file, 1, sz)
@test length(m) == 1
@test m[1] == 0x00
m=nothing; gc()
sz = filesize(file)
# test where offset is actually > than size of file; file is grown with zeroed bytes
m = Mmap.Array(file, 1, sz+1)
@test length(m) == 1
@test m[1] == 0x00
m=nothing; gc()

# Uncomment out once #11351 is resolved
# s = open(file, "r")
# m = Mmap.Array(s)
# @test_throws OutOfMemoryError m[5] = UInt8('x') # tries to setindex! on read-only array
# m=nothing; gc()

s = open(file, "w") do f
    write(f, "Hello World\n")
end

s = open(file, "r")
m = Mmap.Array(s)
close(s)
m=nothing; gc()
m = Mmap.Array(file)
s = open(file, "r+")
c = Mmap.Array(s)
d = Mmap.Array(s)
c[1] = UInt8('J')
Mmap.sync!(c)
close(s)
@test m[1] == UInt8('J')
@test d[1] == UInt8('J')
m=nothing; c=nothing; d=nothing; gc()

s = open(file, "w") do f
    write(f, "Hello World\n")
end

s = open(file, "r")
@test isreadonly(s) == true
c = Mmap.Array(UInt8, (11,), s)
@test c == "Hello World".data
c=nothing; gc()
c = Mmap.Array(UInt8, (UInt16(11),), s)
@test c == "Hello World".data
c=nothing; gc()
@test_throws ArgumentError Mmap.Array(UInt8, (Int16(-11),),  s)
@test_throws ArgumentError Mmap.Array(UInt8, (typemax(UInt),),  s)
close(s)
s = open(file, "r+")
@test isreadonly(s) == false
c = Mmap.Array(UInt8, (11,), s)
c[5] = UInt8('x')
Mmap.sync!(c)
close(s)
s = open(file, "r")
str = readline(s)
close(s)
@test startswith(str, "Hellx World")
c=nothing; gc()

c = Mmap.Array(file)
@test c == "Hellx World\n".data
c=nothing; gc()
c = Mmap.Array(file, 3)
@test c == "Hel".data
c=nothing; gc()
s = open(file, "r")
c = Mmap.Array(s, 6)
@test c == "Hellx ".data
close(s)
c=nothing; gc()
c = Mmap.Array(file, 5, 6)
@test c == "World".data
c=nothing; gc()

s = open(file, "w")
write(s, "Hello World\n")
close(s)

# test Mmap.Array
m = Mmap.Array(file)
t = "Hello World\n"
for i = 1:12
    @test m[i] == t.data[i]
end
@test_throws BoundsError m[13]
m=nothing; gc()

m = Mmap.Array(file,6)
@test m[1] == "H".data[1]
@test m[2] == "e".data[1]
@test m[3] == "l".data[1]
@test m[4] == "l".data[1]
@test m[5] == "o".data[1]
@test m[6] == " ".data[1]
@test_throws BoundsError m[7]
m=nothing; gc()

m = Mmap.Array(file,2,6)
@test m[1] == "W".data[1]
@test m[2] == "o".data[1]
@test_throws BoundsError m[3]
finalize(m); gc()

s = open(file, "w")
write(s, [0xffffffffffffffff,
          0xffffffffffffffff,
          0xffffffffffffffff,
          0x000000001fffffff])
close(s)
s = open(file, "r")
@test isreadonly(s)
b = Mmap.BitArray((17,13), s)
@test b == trues(17,13)
@test_throws ArgumentError Mmap.BitArray((7,3), s)
close(s)
s = open(file, "r+")
b = Mmap.BitArray((17,19), s)
rand!(b)
Mmap.sync!(b)
b0 = copy(b)
close(s)
s = open(file, "r")
@test isreadonly(s)
b = Mmap.BitArray((17,19), s)
@test b == b0
close(s)
finalize(b); finalize(b0)
b = nothing; b0 = nothing
gc()
rm(file)

# mmap with an offset
A = rand(1:20, 500, 300)
fname = tempname()
s = open(fname, "w+")
write(s, size(A,1))
write(s, size(A,2))
write(s, A)
close(s)
s = open(fname)
m = read(s, Int)
n = read(s, Int)
A2 = Mmap.Array(Int, (m,n), s)
@test A == A2
seek(s, 0)
A3 = Mmap.Array(Int, (m,n), s, convert(FileOffset,2*sizeof(Int)))
@test A == A3
A4 = Mmap.Array(Int, (m,150), s, convert(FileOffset,(2+150*m)*sizeof(Int)))
@test A[:, 151:end] == A4
close(s)
finalize(A2); finalize(A3); finalize(A4)
gc()
rm(fname)

# AnonymousMmap
m = Mmap.AnonymousMmap()
@test m.name == ""
@test !m.readonly
@test m.create
@test isopen(m)
@test isreadable(m)
@test iswritable(m)

m = Mmap.Array(UInt8, 12)
@test length(m) == 12
@test all(m .== 0x00)
@test m[1] === 0x00
@test m[end] === 0x00
m[1] = 0x0a
Mmap.sync!(m)
@test m[1] === 0x0a
m = Mmap.Array(UInt8, 12; shared=false)
m = Mmap.Array(Int, 12)
@test length(m) == 12
@test all(m .== 0)
@test m[1] === 0
@test m[end] === 0
m = Mmap.Array(Float64, 12)
@test length(m) == 12
@test all(m .== 0.0)
m = Mmap.Array(Int8, (12,12))
@test size(m) == (12,12)
@test all(m == zeros(Int8, (12,12)))
@test sizeof(m) == prod((12,12))
n = similar(m)
@test size(n) == (12,12)
n = similar(m, (2,2))
@test size(n) == (2,2)
n = similar(m, 12)
@test length(n) == 12
@test size(n) == (12,)

n = similar(m, UInt8)
@test size(n) == size(m)
@test eltype(n) == UInt8
m = Mmap.zeros(UInt8, (12,12))
n = Mmap.zeros(UInt8, (12,12))
@test n == m
