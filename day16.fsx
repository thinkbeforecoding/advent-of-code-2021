open System
open System.Runtime.CompilerServices
#r "System.Buffers"

let data =
    IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input/day16.txt")
    |> Convert.FromHexString

let x = System.Collections.BitArray(data)

let inline mask n =
   (1 <<< n) - 1

[<Struct; IsByRefLike>]
type BitSpan =
    { Span: ReadOnlySpan<byte> 
      Start: int
      Length: int
    }

    static member ofArray (bytes: byte[]) =
        { Span = Span.op_Implicit (bytes.AsSpan())
          Start = 0
          Length = bytes.Length*8 }

    static member ofString (s: String) =
        BitSpan.ofArray(Convert.FromHexString s)

    member this.Slice(skip) =
        let totalSkip = this.Start + skip
        let skipBytes = totalSkip >>> 3
        let newStart = totalSkip &&& 7
        { Span = this.Span.Slice(skipBytes)
          Start = newStart
          Length = this.Length - skip }

    member this.Slice(skip, len) =
        let totalSkip = this.Start + skip
        let skipBytes = totalSkip >>> 3
        let newStart = totalSkip &&& 7
        let totalEnd = this.Start + skip + len
        let endBytes = (totalEnd+7) >>> 3
        let lenBytes = endBytes - skipBytes
        { Span = this.Span.Slice(skipBytes, lenBytes)
          Start = newStart
          Length = len }

    member this.OneBit() =
        let b = this.Span[0]
        int ((b >>> (7-this.Start)) &&& 1uy)

    member this.Bits(n) =
        let v = 
            if this.Start+n <= 8 then
                int this.Span[0] 
            elif this.Start+n <= 16 then
                ((int this.Span[0]) <<< 8) ||| int this.Span[1]
            elif this.Start+n <= 24 then
                ((int this.Span[0]) <<< 16) ||| ((int this.Span[1]) <<< 8) ||| (int this.Span[2])
            else
                failwith "n should be smaller than 16"

        (v >>> (8 - (this.Start+n) &&& 7)) &&& mask n




// 01010101 01010101 01010101
//    ^     +         +    ^ 
// start = 3 length = 20
// skip = 5 len = 10
// totalSkip = 8 skipBytes = 1 newStart = 0
// totalEnd = 18 

type Op =
    | Sum
    | Product
    | Min
    | Max
    | GreaterThan
    | LessThan
    | Equal

type Packet =
    { Version: int
      Data: Data }
and Data =
    | Literal of int
    | Operator of Op * Packet list



let test() =
    let b = BitSpan.ofArray("Hello"B)
    printfn "%d" b.Length
    let b' = b.Slice(3)
    printfn "%d; %d; %s ; %d" b'.Start b'.Length (Convert.ToHexString (b'.Span.ToArray())) (b'.OneBit())
    let b'' = b'.Slice(5,10)
    printfn "%d;%d:%A" b''.Start b''.Length (b''.Span.ToArray())


test()

let getBit n l =
    let c= BitSpan.ofArray([|0x7Auy;0x55uy|])
    printfn "%B" (c.Slice(n).Bits(l))


let rec parseLiteral (bs: BitSpan) v bitCount =
    let bits = bs.Bits(5)
    let newV = (v <<< 4) ||| (bits &&& 0x0f)
    if bits &&& 0b10000 = 0 then
        struct(newV, bitCount+5)
    else
        parseLiteral (bs.Slice(5)) newV (bitCount+5)




let rec parse (bs: BitSpan) =
    let version = bs.Bits(3)
    let typ = bs.Slice(3).Bits(3)
    if typ = 4 then
        // literal
        let struct(v, bitCount) = parseLiteral (bs.Slice(6)) 0 0
        struct({ Version = version; Data = Literal v },  6+bitCount)
    else
        let op =
            match typ with
            | 0 -> Sum
            | 1 -> Product
            | 2 -> Min
            | 3 -> Max
            | 5 -> GreaterThan
            | 6 -> LessThan
            | 7 -> Equal
            | _ -> failwith "Unknown op"
        // operator
        let lenTyp = bs.Slice(6).Bits(1)
        if lenTyp = 0 then
            // 15bits  totalLength
            let totalLength = bs.Slice(7).Bits(15)
            let packets = parseMultiple (bs.Slice(7+15, totalLength)) []

            struct( { Version = version; Data = Operator packets}, 7+15+totalLength)
        else
            // 11bits packet count
            let packetCount = bs.Slice(7).Bits(11)
            let struct(packets, packetsLen) = parsePackets (bs.Slice(7+11)) packetCount [] 0
            struct ( { Version = version; Data = Operator packets}, 7+11+packetsLen)

and parseMultiple (bs: BitSpan) acc =
    if bs.Length = 0 then
        List.rev acc
    else
        let struct(packet, len) = parse(bs)
        parseMultiple(bs.Slice(len)) (packet :: acc)
and parsePackets (bs: BitSpan) count acc totalLen =
    if count = 0 then
        struct(List.rev acc, totalLen)
    else
        let struct(packet, len) = parse(bs)
        parsePackets (bs.Slice(len)) (count-1) (packet :: acc) (totalLen + len)

let p = parse (BitSpan.ofArray(Convert.FromHexString "D2FE28"))

let o = parse (BitSpan.ofString "38006F45291200")

let o' = parse (BitSpan.ofString "EE00D40C823060")

let pack = parse (BitSpan.ofArray data)

let rec sum packet =
    match packet.Data with
    | Literal _ -> packet.Version
    | Operator subpackets -> 
        packet.Version + List.sumBy sum subpackets

sum (fst (pack.ToTuple()))




