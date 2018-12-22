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

    let toUpper c = 
        Char.ToUpper(c, CultureInfo.CurrentCulture)

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
            match dir with
            | Vertical when btwn (rS + wordLen + 1) -> board.[rS + wordLen + 1, cS] = ' '
            | Vertical -> board.[rS + wordLen, cS] = ' ' || toUpper board.[rS + wordLen, cS] = toUpper word.[wordLen]
            | Horizontal when btwn (cS + wordLen + 1) -> board.[rS, cS + wordLen + 1] = ' '
            | Horizontal -> board.[rS, cS + wordLen] = ' ' || toUpper board.[rS, cS + wordLen] = toUpper word.[wordLen]

        let hasFreeSpace (r:int) (c:int) :bool =
            match dir, r, c with
            // For horizontal words, no neighbor (row - 1 and row + 1) overlap unless we are at a valid overlap position
            | Horizontal, r, c when r = minB || r = maxB -> true
            | Horizontal, r, c -> 
                btwn (r - 1)
                && btwn (r + 1)
                && board.[r - 1, c] = ' ' && board.[r + 1, c] = ' '
            //For vertical words, no neighbor (column - 1 and column + 1) overlap unless we are at a valid overlap position
            | Vertical, r, c when c = minB || c = maxB -> true
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
                    let res = (boardChar = ' ' && hasSpace) || boardChar = ltr
                    if res then
                        true
                    else 
                        false
                )
                |> Seq.filter (fun x -> x = false)
                |> Seq.isEmpty

    let validCoords (coords:BoardCoord) (word:Word) (dir:LayoutDir) (board:Board) =
        if not(board |> inbounds coords) then
            false
        else
            let wl = word.Length - 1
            match coords, dir with
            | (r, c), Vertical -> 
                board |> inbounds (r + wl, c) 
                && isWordPosEmpty coords word dir board
            | (r, c), Horizontal ->
                board |> inbounds (r, c + wl) 
                && isWordPosEmpty coords word dir board

    let findIntersectingPoints (board:Board) (word:Word) =
        // return a list of valid candidate intersection points

        let wl = word.Length

        let (b, e) = getBoardBounds board

        let r = new System.Random()

        seq {
            // for every letter in the input word, search the whole array for a fitting spot
            for i = 0 to wl - 1 do
                for r = b to e - 1 do
                    for c = b to e - 1 do
                        // found intersecting point
                        if toUpper board.[r, c] = toUpper word.[i] then 
                            // is it valid for a horizontal layout?
                            let minHorCoord = BoardCoord(r, c - i)
                            if validCoords minHorCoord word Horizontal board then
                                yield minHorCoord, Horizontal
                            else
                                // is it valid for a vertical layout?
                                let minVerCoord = BoardCoord(r - i, c)
                                if validCoords minVerCoord word Vertical board then
                                    yield minVerCoord, Vertical
            }

    let private layoutWordInternal (coords:BoardCoord) (dir:LayoutDir) (word:Word) (board:Board) :Board =
        let wordLen = word.Length - 1
        let newBoard = board |> Array2D.copy
        
        match coords, dir with
        | ((r, c), Horizontal) ->
            for c1 = c to c + wordLen do
                newBoard.[r, c1] <- word.[c1 - c]
            newBoard
        | ((r, c), Vertical) ->
            for r1 = r to r + wordLen do
                newBoard.[r1, c] <- word.[r1 - r]
            newBoard

    let public layoutWord (coords:BoardCoord) (dir:LayoutDir) (word:Word) (board:Board) :Board option =
        match word with
        | null -> None    // can't layout a null word!
        | "" -> None    // can't layout an empty string
        | word when word.Length < 2 -> None // can't be only a single character long
        | word when not (validCoords coords word dir board) -> None // can't be placed somewhere illegal
        | word ->
            Some(layoutWordInternal coords dir word board)

    let addWordToPuzzle (puzzle:Puzzle) (word:InputWord) :seq<Puzzle> =
        seq {
            match puzzle with
            | (board, wordCount, wordList) when wordCount = 0 -> 
                // add initial word to crossword puzzle
                let coords = board |> centerWord Horizontal word.Word // get the coordinates for a center placement of the word
                let newBoard = board |> layoutWordInternal coords Horizontal word.Word
                let r = [{Word = word.Word; Hint = word.Hint; Coord = coords; Dir = Horizontal}]
                yield (newBoard, 1, r)
            | (board, wordCount, words) when wordCount > 0 ->
                let res = findIntersectingPoints board word.Word

                for (coords, dir) in res do
                    let newBoard = board |> layoutWordInternal coords dir word.Word
                    let wordList = {Word = word.Word; Hint = word.Hint; Coord = coords; Dir = dir} :: words
                    yield (newBoard, wordCount + 1, wordList)
        }

    let createEmptyPuzzle (words:InputWords) :Puzzle =
        let arrayDim = (words |> Seq.map (fun x -> x.Word.Length ) |> Seq.max) * 3
        let board = Array2D.create arrayDim arrayDim ' '
        (board, 0, [])


    let fixSpacesInWords (words:InputWords) =
        words |> Seq.map (fun r -> {Word = r.Word.Replace(" ", Convert.ToString(SpaceChar)); Hint = r.Hint})

    let joinWith (rowDivider:String) (input: String[,])  = 
        let sb = new StringBuilder()
        for r = (Array2D.base1 input) to (Array2D.length1 input) - 1 do
            for c  = (Array2D.base1 input) to (Array2D.length1 input) - 1 do
                sb.Append(input.[r, c]) |> ignore
            sb.Append(rowDivider) |> ignore
        sb.ToString()

    let puzzleToString (puzzle:Puzzle) :string =
        match puzzle with
        | (board, wordCount, words) ->
            let nl = Environment.NewLine

            let puzzleHeader = "Puzzle:"
            let puzzle = 
                board 
                |> Array2D.map (fun c -> if c = SpaceChar then " " else sprintf "%c" c) 
                |> joinWith Environment.NewLine

            let wordListHeader = sprintf "Word List (%d):" wordCount

            let hintGroups = 
                words 
                |> List.groupBy (fun x -> x.Dir) 
                |> List.sortBy (fun (dir, list) -> dir) 
                |> List.map (fun (dir, lst) -> 
                    (dir.ToString() + ": ") :: 
                    (lst 
                        |> List.sortBy (fun pw -> pw.Coord) 
                        |> List.map (fun pw -> 
                            let r,c = pw.Coord
                            sprintf "(%d,%d)   %s" r c pw.Hint)))
                |> List.concat
            let wordList = String.Join(Environment.NewLine, hintGroups)

            let output = String.Join(nl + nl, [puzzleHeader; puzzle; wordListHeader; wordList])
            String.Join(Environment.NewLine, output)

    let shrinkPuzzleToSmallest (emptyChar:Char) (puzzle:Puzzle) :Puzzle = puzzle
        // TODO: Find the number of rows before the content to be removed
        // TODO: Find the number of rows after the content to be removed
        // TODO: Find the number of columns before the content to be removed
        // TODO: Find the number of columns after the content to be removed
        // TODO: Rewrite all of the PuzzleWord indices so that the words are
        // still referenced at the correct locations
        
        // Use Array2D.init

    let rec addWordsToPuzzle (words:InputWords) (puzzle:Puzzle) :seq<Puzzle> =
        seq {
            match words with
            | [] -> yield puzzle
            | head :: tail -> 
                for newPuzzle in addWordToPuzzle puzzle head do
                    yield! addWordsToPuzzle tail newPuzzle
        }

    // this method is NOT recursive
    // it takes the master list of words and starting puzzle
    // then returns a sequence of puzzles that meet minimum criteria
    let createPuzzles (words:InputWords) (puzzle:Puzzle) :seq<Puzzle> =
        let spaceFixedWords = fixSpacesInWords words |> Seq.toList
        let res = addWordsToPuzzle spaceFixedWords puzzle
        res 
        |> Seq.where (fun (board, wordCount, puzzleWords) -> wordCount = words.Length)
        |> Seq.map (fun (board, wordCount, puzzleWords) -> 
                shrinkPuzzleToSmallest SpaceChar (board, wordCount, puzzleWords |> List.rev)
            )