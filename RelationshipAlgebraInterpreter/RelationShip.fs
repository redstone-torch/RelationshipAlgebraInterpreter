module RelationShip

open System
open System.Text

/// 解释器所支持的数据类型 其中Unit只是作为空占位 并不实际使用
/// 
/// 在实际的表中 每个类型的值的集合都要加上一个NULL值
type RAIType =
    | TypeNumber
    | TypeString
    | TypeBool
    | TypeUnit

/// 获取对应的实际类型
let getCLRType = function
    | TypeNumber -> typeof<Double>
    | TypeString -> typeof<String>
    | TypeBool -> typeof<Boolean>
    | TypeUnit -> typeof<Unit>

/// 通过对象获取对应的RAIType 如果对象的类型不支持 那么返回TypeUnit
let getRAIType (o: obj) =
    match o with
    | :? double -> TypeNumber
    | :? string -> TypeString
    | :? bool -> TypeBool
    | _ -> TypeUnit

/// 字段的属性
type RAIAttribute = {
    Name: string
    Type: RAIType
}

type Tuple = obj list

type Relationship = {
    Attributes: RAIAttribute list
    Tuples: Tuple list
}

//type IRelationship =
//    abstract member Attributes: RAIAttribute list with get
//    abstract member Tuples: Tuple seq with get

//type SeqRelationship = {
//    AttributeList: RAIAttribute list
//    TupleSeq: Tuple seq
//} with
//    interface IRelationship with
//        override this.Attributes with get() = this.AttributeList
//        override this.Tuples with get() = this.TupleSeq

//type Table = {
//    AttributeList: RAIAttribute list
//    TupleList: Tuple list
//} with
//    interface IRelationship with
//        override this.Attributes with get() = this.AttributeList
//        override this.Tuples with get() = Seq.ofList this.TupleList


/// 打印关系
let relationshipToString (r: Relationship) =

    /// 将属性和元组一起转换成二维列表
    let string2DList =

        let attributeString2DList =
            r.Attributes
            |> List.map (fun x -> x.Name)
            |> List.singleton
        
        let tupleString2DSeq =

            let toString (o: obj) =
                match o with
                | null -> "null"
                | :? string -> $"\"{o}\""
                | _ -> o.ToString()

            r.Tuples
            |> Seq.map (List.map toString)

        tupleString2DSeq
        |> Seq.append attributeString2DList
        |> Seq.toList

    /// 二维列表每一列的最大长度
    let eachColumnMaxLengthList =

        let countMaxLength =
            List.fold (fun maxLength str -> if String.length str > maxLength then String.length str else maxLength) 0
        
        string2DList
        |> List.transpose
        |> List.map countMaxLength

    /// 每行之间的分割线
    let dividingLine =

        let generateLine (builder: StringBuilder) len =
            for _ in 0 .. len do
                builder.Append('-') |> ignore
            builder.Append("-+")

        let stringBuilder =
            eachColumnMaxLengthList
            |> List.fold generateLine (StringBuilder("+"))

        stringBuilder.ToString()


    let output =

        let stringBuilder = StringBuilder()

        stringBuilder.AppendLine(dividingLine) |> ignore

        for stringList in string2DList do
            
            stringBuilder.Append("| ") |> ignore
            
            for maxLength, str in (Seq.zip eachColumnMaxLengthList stringList) do
            
                stringBuilder.Append(str) |> ignore
                
                for _ in 1 .. maxLength - str.Length do
                    stringBuilder.Append(' ') |> ignore
            
                stringBuilder.Append(" | ") |> ignore
            
            stringBuilder.AppendLine() |> ignore
            
            stringBuilder.AppendLine(dividingLine) |> ignore

        stringBuilder.ToString()

    output