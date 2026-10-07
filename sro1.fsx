// ===== Готовый фрагмент: чтение CSV =====
let readRows (fileName: string) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)
    System.IO.File.ReadAllLines path
    |> Array.toList
    |> List.tail
    |> List.filter (fun line -> line.Trim() <> "")
    |> List.map (fun line -> line.Split ';' |> Array.map (fun cell -> cell.Trim()))

let toFloat (text: string) = float (text.Replace(',', '.'))

let toDate (text: string) =
    System.DateTime.ParseExact(text, [| "yyyy-MM-dd"; "dd.MM.yyyy" |], null, System.Globalization.DateTimeStyles.None)

let showDate (date: System.DateTime) = date.ToString "dd.MM.yyyy"
// ===== Конец готового фрагмента =====

// 1. Описание записи Price
type Price = {
    Date: System.DateTime
    Shop: string
    Product: string
    Category: string
    Price: float
    IsPromo: bool
}

// Парсинг массива ячеек в тип Price
let parsePrice (cells: string[]) : Price = {
    Date = toDate cells[0]
    Shop = cells[1]
    Product = cells[2]
    Category = cells[3]
    Price = toFloat cells[4]
    IsPromo = cells[5] = "1"
}

let prices = readRows "data.csv" |> List.map parsePrice
