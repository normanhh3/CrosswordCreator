module CrosswordCreator

    open System    
    open System.Text
    open System.Globalization

    type BoardCoord = int * int
    type Word = string
    type Words = List<Word>
    type WordCount = int
    type LayoutDir = Horizontal | Vertical
    type Cell = char
    type Board = Cell[,]
    type InputWord = { Word: Word; Hint: string }
    type InputWords = List<InputWord>

    type PuzzleWord = { Word: Word; Hint: string; Coord: BoardCoord; Dir: LayoutDir }
    type PuzzleWords = List<PuzzleWord>

    type Puzzle = Board * WordCount * PuzzleWords

    type Rank = int
    type IntersectionPoint = BoardCoord * LayoutDir * Rank

    let centerWord (dir:LayoutDir) (word:Word) (board:Board) :BoardCoord =
        let halfOfLen = (board |> Array2D.length1) / 2
        (halfOfLen, halfOfLen - (word.Length / 2))

    let between min max value =
        not(value < min || value > max)

    let getBoardBounds (board:Board) =
        (board |> Array2D.base1, (board |> Array2D.length1) - 1)

    let inbounds (r, c) (board:Board) :bool =
        let (alpha, omega) = getBoardBounds board
        (between alpha omega r) && (between alpha omega c)

    let isWordPosEmpty (coords:BoardCoord) (word:Word) (dir:LayoutDir) (board:Board) :bool =
        let wordLen = word.Length
        let b = getBoardBounds board 
        let (rS, cS) = coords
        let cu = CultureInfo.CurrentCulture

        let getOverlapPoints = 
            let rowOrColumnSeq = 
                match dir with
                | Horizontal -> (seq { for c in cS .. (wordLen - 1) do yield rS,c })
                | Vertical -> (seq { for r in rS .. (wordLen - 1) do yield r,cS })

            seq { for i in 0 .. wordLen - 1 do yield i }
                |> Seq.zip rowOrColumnSeq
                |> Seq.map (fun ((r, c), i) ->
                    board.[r,c] = ' ' || Char.ToUpper(board.[r,c], cu) = Char.ToUpper(word.[i],cu)
                )
        
        // TODO: no two consecutive positions should both have content already in them
        let res = 
            getOverlapPoints 
                |> Seq.filter (fun x -> x = false)
                |> Seq.toArray

        res
            |> Seq.length > 1

    let validCoords (coords:BoardCoord) (word:Word) (dir:LayoutDir) (board:Board) =
        if not(board |> inbounds coords) then
            false
        else
            let wl = word.Length
            match coords, wl, dir with
            | (r, c), _, Vertical -> 
                board |> inbounds (r + wl, c) 
                && isWordPosEmpty coords word dir board
            | (r, c), _, Horizontal ->
                board |> inbounds (r, c + wl) 
                && isWordPosEmpty coords word dir board

    let findIntersectingPoints (board:Board) (word:Word) =
        // return a rank ordered list of valid candidate intersection points

        let wl = word.Length

        let (b,e) = getBoardBounds board

        let getPoints = 
            seq {
            // for every letter in the input word, search the whole array for a fitting spot
            for i = 0 to wl - 1 do
                for r = b to e - 1 do
                    for c = b to e - 1 do
                        // found intersecting point
                        if board.[r,c] = word.[i] then 
                            // is it valid for a horizontal layout?
                            let minHorCoord = (r, c - i)
                            if validCoords minHorCoord word Horizontal board then
                                yield minHorCoord, Horizontal, 0
                            else
                                // is it valid for a vertical layout?
                                let minVerCoord = (r - i, c)
                                if validCoords minVerCoord word Vertical board then
                                    yield minVerCoord, Vertical, 0
            }
        getPoints
         

    let layoutWord (coords:BoardCoord) (dir:LayoutDir) (word:Word) (board:Board) :Board option =
        match word with
        | null -> None    // can't layout a null word!
        | word ->
            let wordLen = word.Length
            if wordLen < 2 && not (board |> validCoords coords word dir)
                // too short or invalid coordinates - can't place it!
                then None
                else
                    let newBoard = board |> Array2D.copy
                    
                    match coords, dir with
                    | ((r, c), Horizontal) ->
                        for c1 = c to c + wordLen - 1 do
                            newBoard.[r, c1] <- word.[c1 - c]
                        Some(newBoard)
                    | ((r, c), Vertical) ->
                        for r1 = r to r + wordLen - 1 do
                            newBoard.[r1, c] <- word.[r1 - r]
                        Some(newBoard)

    let addWordToPuzzle (puzzle:Puzzle option) (word:InputWord) :Puzzle option =
        match puzzle with
        | None -> None
        | Some(board, wordCount, words) when wordCount = 0 -> 
            // add initial word to crossword puzzle
            let coords = board |> centerWord Horizontal word.Word // get the coordinates for a center placement of the word
            let newBoard = board |> layoutWord coords Horizontal word.Word
            match newBoard with
            | Some b -> 
                let r = [{Word = word.Word; Hint = word.Hint; Coord = coords; Dir = Horizontal}]
                Some((b, 1, r))
            | None -> None
        | Some(board, wordCount, words) when wordCount > 0 ->
            
            // find ranked intersection points for new word against existing words
            let res = findIntersectingPoints board word.Word

            if Seq.isEmpty res then
                Some(board, wordCount, words)
            else
                // probably should recurse down through the options available
                // picking the first available location right now
                let (coords, dir, rank) = res |> Seq.head
                let newBoard = board |> layoutWord coords dir word.Word
                match newBoard with
                | Some(b) -> 
                    let wordList = {Word = word.Word; Hint = word.Hint; Coord = coords; Dir = dir} :: words
                    Some(b, wordCount + 1, wordList)
                | None -> None

    let createPuzzle (words:InputWords) :Puzzle option =
        let arrayDim = (words |> Seq.map (fun x -> x.Word.Length ) |> Seq.max) * 3
        let board = Array2D.create arrayDim arrayDim ' '
        let initialPuzzle = Some(board, 0, [])
        words |> Seq.fold addWordToPuzzle initialPuzzle

    let joinWith (rowDivider:String) (input: String[,])  = 
        let sb = new StringBuilder()
        for r = (Array2D.base1 input) to (Array2D.length1 input) - 1 do
            for c  = (Array2D.base1 input) to (Array2D.length1 input) - 1 do
                sb.Append(input.[r, c]) |> ignore
            sb.Append(rowDivider) |> ignore
        sb.ToString()

    let puzzleToString (puzzle:Puzzle option) :string =
        match puzzle with
        | None -> "No puzzle to draw!"
        | Some(board, wordCount, words) ->
            let nl = Environment.NewLine

            let puzzleHeader = "Puzzle:"
            let puzzle = 
                board |> 
                Array2D.map (sprintf "%c") |> 
                joinWith Environment.NewLine

            let wordListHeader = sprintf "Word List (%d):" wordCount

            let hintGroups = 
                words |> 
                List.groupBy (fun x -> x.Dir) |> 
                List.sortBy (fun (dir, list) -> dir) |>
                List.map (fun (dir, lst) -> (dir.ToString() + ": ") :: (lst |> List.map (fun pw -> "  " + pw.Word))) |>
                List.concat
            let wordList = String.Join(Environment.NewLine, hintGroups)

            let output = String.Join(nl + nl, [puzzleHeader; puzzle; wordListHeader; wordList])
            String.Join(Environment.NewLine, output)