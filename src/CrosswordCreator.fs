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

    let SpaceChar = '^'

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

    // This function assumes that the word will fit on the board given the coordinates
    let isWordPosEmpty (coords:BoardCoord) (word:Word) (dir:LayoutDir) (board:Board) :bool =
        let wordLen = word.Length - 1
        let (minB, maxB) = getBoardBounds board 
        let (rS, cS) = coords
        let btwn = between minB maxB

        let rowOrColumnSeq = 
            match dir with
            | Horizontal -> (seq { for c in cS .. (cS + wordLen) do yield rS, c })
            | Vertical -> (seq { for r in rS .. (rS + wordLen) do yield r, cS })

        // TODO: no two consecutive positions should both have non-space characters already in them

        let toUpper c = 
            Char.ToUpper(c, CultureInfo.CurrentCulture)

        // this implementation doesn't need guards on the match expressions because the 
        // coordinates have already been boxed to the board dimensions before we get to this code
        let hasLeadingSpace = 
            match dir, coords with
            | Vertical, (0, c) -> true
            | Vertical, (r, c) -> board.[r - 1, c] = ' '
            | Horizontal, (r, 0) -> true
            | Horizontal, (r, c) -> board.[r, c - 1] = ' '


        // this implementation doesn't need guards on the match expressions because the 
        // coordinates have already been boxed to the board dimensions before we get to this code
        let hasTrailingSpace =
            let (rL, cL) =
                match dir with
                | Vertical when btwn (rS + wordLen + 1) -> (rS + wordLen + 1, cS)
                | Vertical -> (rS + wordLen, cS)
                | Horizontal when btwn (cS + wordLen + 1) -> (rS, cS + wordLen + 1)
                | Horizontal -> (rS, cS + wordLen)
            board.[rL, cL] = ' '

        let hasFreeSpace (r:int) (c:int) :bool =
            match dir, r, c with
            // For horizontal words, no neighbor (row - 1 and row + 1) overlap unless we are at a valid overlap position
            | Horizontal, r, c when r = minB -> true
            | Horizontal, r, c when r = maxB -> true
            | Horizontal, r, c -> 
                btwn (r - 1)
                && btwn (r + 1)
                && board.[r - 1, c] = ' ' && board.[r + 1, c] = ' '
            //For vertical words, no neighbor (column - 1 and column + 1) overlap unless we are at a valid overlap position
            | Vertical, r, c when r = minB || c = maxB -> true
            | Vertical, r, c -> 
                btwn (c - 1)
                && btwn (c + 1)
                && board.[r, c - 1] = ' ' && board.[r, c + 1] = ' '

        if not (hasLeadingSpace && hasTrailingSpace) then
            false
        else
            word
                |> Seq.map toUpper // convert the input word into uppercase for comparison
                |> Seq.zip rowOrColumnSeq   // zip together with the auto generated pairs of coordinates mapped to the target position
                |> Seq.map (fun ((r, c), ltr) ->
                    let boardChar = toUpper board.[r, c]
                    let hasSpace = hasFreeSpace r c
                    (boardChar = ' ' && hasSpace) || boardChar = ltr   
                )
                |> Seq.filter (fun x -> x = false)
                |> Seq.isEmpty

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

        let r = new System.Random()

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
            |> Seq.map (fun (c, dir, weight) -> c, dir, r.Next())
            |> Seq.sortBy (fun (c, dir, weight) -> weight)

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
        words 
            |> Seq.map (fun r -> {Word = r.Word.Replace(" ", Convert.ToString(SpaceChar)); Hint = r.Hint})
            |> Seq.fold addWordToPuzzle initialPuzzle

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
                board 
                |> Array2D.map (fun c -> if c = SpaceChar then " " else sprintf "%c" c) 
                |> joinWith Environment.NewLine

            let wordListHeader = sprintf "Word List (%d):" wordCount

            let hintGroups = 
                words |> 
                List.groupBy (fun x -> x.Dir) |> 
                List.sortBy (fun (dir, list) -> dir) |>
                List.map (fun (dir, lst) -> (dir.ToString() + ": ") :: (lst |> List.map (fun pw -> "   " + pw.Hint))) |>
                List.concat
            let wordList = String.Join(Environment.NewLine, hintGroups)

            let output = String.Join(nl + nl, [puzzleHeader; puzzle; wordListHeader; wordList])
            String.Join(Environment.NewLine, output)


    let shrinkBoardToSmallest (emptyChar:Char) (board:Board) :Board = board
    //    let (board, wc, wors) = puzzle