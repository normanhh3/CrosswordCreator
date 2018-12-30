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

    type PuzzleWord = { Word: Word; Hint: string; Coord: BoardCoord; Dir: LayoutDir; Index: int }
    type PuzzleWords = List<PuzzleWord>
    (*
        property Tail:  PuzzleWord list
        property Length:  int
        property Item:  PuzzleWord
        property IsEmpty:  bool
        property Head:  PuzzleWord
        static property Empty:  PuzzleWord list
    *)

    type Puzzle = Board * WordCount * PuzzleWords

    type Rank = int
    type IntersectionPoint = BoardCoord * LayoutDir * Rank

    let SpaceChar = '^'
    let EmptyChar = ' '
    let BoxChar = '\u2610'

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
            | Vertical, (r, c) -> board.[r - 1, c] = EmptyChar
            | Horizontal, (r, 0) -> true
            | Horizontal, (r, c) -> board.[r, c - 1] = EmptyChar

        // this implementation doesn't need guards on the match expressions because the 
        // coordinates have already been boxed to the board dimensions before we get to this code
        let hasTrailingSpace =
            match dir with
            | Vertical when btwn (rS + wordLen + 1) -> board.[rS + wordLen + 1, cS] = EmptyChar
            | Vertical -> board.[rS + wordLen, cS] = EmptyChar || toUpper board.[rS + wordLen, cS] = toUpper word.[wordLen]
            | Horizontal when btwn (cS + wordLen + 1) -> board.[rS, cS + wordLen + 1] = EmptyChar
            | Horizontal -> board.[rS, cS + wordLen] = EmptyChar || toUpper board.[rS, cS + wordLen] = toUpper word.[wordLen]

        let hasFreeSpace (r:int) (c:int) :bool =
            match dir, r, c with
            // For horizontal words, no neighbor (row - 1 and row + 1) overlap unless we are at a valid overlap position
            | Horizontal, r, c when r = minB || r = maxB -> true
            | Horizontal, r, c -> 
                btwn (r - 1)
                && btwn (r + 1)
                && board.[r - 1, c] = EmptyChar && board.[r + 1, c] = EmptyChar
            //For vertical words, no neighbor (column - 1 and column + 1) overlap unless we are at a valid overlap position
            | Vertical, r, c when c = minB || c = maxB -> true
            | Vertical, r, c -> 
                btwn (c - 1)
                && btwn (c + 1)
                && board.[r, c - 1] = EmptyChar && board.[r, c + 1] = EmptyChar

        if not (hasLeadingSpace && hasTrailingSpace) then
            false
        else
            word
                |> Seq.map toUpper // convert the input word into uppercase for comparison
                |> Seq.zip rowOrColumnSeq   // zip together with the auto generated pairs of coordinates mapped to the target position
                |> Seq.map (fun ((r, c), ltr) ->
                    let boardChar = toUpper board.[r, c]
                    let hasSpace = hasFreeSpace r c
                    let res = (boardChar = EmptyChar && hasSpace) || boardChar = ltr
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
        let (b, e) = getBoardBounds board

        seq {
            // for every letter in the input word, search the whole array for a fitting spot
            for i = 0 to word.Length - 1 do
                for r = b to e do
                    for c = b to e do
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
                let r = [{Word = word.Word; Hint = word.Hint; Coord = coords; Dir = Horizontal; Index = wordCount+1}]
                yield (newBoard, 1, r)
            | (board, wordCount, words) when wordCount > 0 ->
                let res = findIntersectingPoints board word.Word

                for (coords, dir) in res do
                    let newBoard = board |> layoutWordInternal coords dir word.Word
                    let wordList = {Word = word.Word; Hint = word.Hint; Coord = coords; Dir = dir; Index = wordCount+1} :: words
                    yield (newBoard, wordCount + 1, wordList)
            | _ -> yield puzzle // this appears necessary to satisfy the compiler that we really have covered all cases
        }

    let createEmptyPuzzle (words:InputWords) :Puzzle =
        let arrayDim = (words |> Seq.map (fun x -> x.Word.Length ) |> Seq.max) * 3
        let board = Array2D.create arrayDim arrayDim EmptyChar
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

    let forAllColumnsInRowAreEmpty row (board:Board) b e =
        seq { for col in b .. e do yield board.[row,col] }
        |> Seq.forall (fun c -> c = EmptyChar)

    let forAllRowsInColumnAreEmpty col (board:Board) b e =
        seq { for row in b .. e do yield board.[row,col] }
        |> Seq.forall (fun c -> c = EmptyChar)

    let shrinkPuzzleToSmallest (puzzle:Puzzle) :Puzzle =
        let getTrueCount seq =
            seq 
            |> Seq.takeWhile (fun r -> r = true) 
            |> Seq.length

        match puzzle with
        | (board, wordCount, puzzleWords) -> 

            let (b,e) = getBoardBounds board

            let emptyLeadingRows = 
                seq { for row in b .. e do yield forAllColumnsInRowAreEmpty row board b e } |> getTrueCount

            let emptyTrailingRows = 
                seq { for row in e .. -1 .. b do yield forAllColumnsInRowAreEmpty row board b e } |> getTrueCount

            let emptyLeadingColumns = 
                seq { for col in b .. e do yield forAllRowsInColumnAreEmpty col board b e } |> getTrueCount

            let emptyTrailingColumns = 
                seq { for col in e .. -1 .. b do yield forAllRowsInColumnAreEmpty col board b e } |> getTrueCount

            // TODO: Something about this block of code is causing the issue with the one failing test
            // We are taking too many
            let newRows = e - emptyLeadingRows - emptyTrailingRows + 1
            let newColumns = e - emptyLeadingColumns - emptyTrailingColumns + 1

            let newSquareDim = 
                if newRows < newColumns then 
                    newColumns 
                else 
                    newRows

            // Transform indices of puzzle words to match smaller board dimensions
            let newPuzzleWordList =
                puzzleWords 
                |> Seq.map (fun pw ->
                    let (r,c) = pw.Coord
                    {pw with Coord = (r - emptyLeadingRows, c - emptyLeadingColumns)}
                )
                |> Seq.toList
            
            // Create the new board with square dimensions that fit the largest dimension
            let newBoard =
                let newB = Array2D.create newSquareDim newSquareDim EmptyChar
                let srcRow = emptyLeadingRows
                let srcCol = emptyLeadingColumns
                Array2D.blit board srcRow srcCol newB 0 0 newSquareDim newSquareDim
                newB

            (newBoard, wordCount, newPuzzleWordList)

    let rec addWordsToPuzzle (words:InputWords) (puzzle:Puzzle) :seq<Puzzle> =
        seq {
            match words with
            | [] -> yield puzzle
            | head :: tail -> 
                for newPuzzle in addWordToPuzzle puzzle head do
                    yield! addWordsToPuzzle tail newPuzzle
        }


    let puzzleToString (puzzle:Puzzle) :string =
        match puzzle with
        | (board, wordCount, words) ->
            let nl = Environment.NewLine

            let puzzleHeader = "Puzzle:"
            let puzzle = 
                board 
                |> Array2D.map (fun c -> 
                    if c = SpaceChar then " " else sprintf "%c" c) 
                |> joinWith Environment.NewLine

            let wordListHeader = sprintf "Word List (%d):" wordCount

            let hintGroups = 
                words 
                |> List.groupBy (fun x -> x.Dir) 
                |> List.sortBy (fun (dir, list) -> dir) 
                |> List.map (fun (dir, lst) -> 
                    (dir.ToString() + ":") :: 
                    (lst 
                        |> List.sortBy (fun pw -> pw.Coord) 
                        |> List.map (fun pw -> 
                            let r,c = pw.Coord
                            sprintf "(%d,%d)\t%s" r c pw.Hint)))
                |> List.concat
            let wordList = String.Join(Environment.NewLine, hintGroups)

            let output = String.Join(nl + nl, [puzzleHeader; puzzle; wordListHeader; wordList])
            String.Join(Environment.NewLine, output)

    // Todo: Implement smart tabbing through the characters so that tabbing moves through a complete word

    let puzzleToHtml puzzle title =
        match puzzle with
        | (board, wordCount, words) ->
            let (b,e) = getBoardBounds board
            String.Join(
                Environment.NewLine,
                seq {
                    yield "<html>"
                    yield "<head>"
                    yield "<style>"
                    yield """
    h1 {
        text-align: center;
    }
    table {
        text-align: center;
        margin-left: auto;
        margin-right: auto;
    }
    div.hint span:first-child {
        font-weight: bold;
        margin-right: 1em;
    }
    div.hint span:last-child {
        
    }
    td input {
        border: 0;
        margin: .5em;
    }
                    """
                    yield "</style>"
                    yield "</head>"
                    yield "<body>"
                    yield sprintf """<h1>%s</h1>""" title
                    yield """<table id="puzzle">"""
                    for r in b .. e do
                        yield "<tr>"
                        yield String.Join(
                            String.Empty,
                            seq {
                                for c in b .. e do
                                    let tdStyle = 
                                        """ style="border: 1px solid black; margin: .25em;" """
                                    yield
                                        if board.[r,c] = BoxChar then
                                            match words |> Seq.tryFind (fun w -> w.Coord = (r,c)) with
                                            | Some(w) ->
                                                sprintf """<td%s><input type="text" maxlength="1" size="1" placeholder="%d"></input></td>""" tdStyle w.Index
                                            | None ->
                                                sprintf """<td%s><input type="text" maxlength="1" size="1"></input></td>""" tdStyle
                                        else
                                            "<td>" + board.[r,c].ToString() + "</td>"
                            }
                        )
                        yield "</tr>"
                    yield "</table>"
                    yield "<div>"
                    let hintGroups = 
                        words 
                        |> List.groupBy (fun x -> x.Dir) 
                        |> List.sortBy (fun (dir, list) -> dir)
                        |> List.map (fun (dir, lst) -> 
                            ("<h2>" + dir.ToString() + "</h2>") :: 
                            (lst 
                                |> List.sortBy (fun pw -> pw.Index) 
                                |> List.map (fun pw -> 
                                    let r,c = pw.Coord
                                    sprintf """<div class="hint"><span>%d.</span><span>%s</span></div>""" pw.Index pw.Hint)))
                        |> List.concat
                    yield String.Join(Environment.NewLine, hintGroups)
                    yield "</div>"
                    yield "</body>"
                    yield "</html>"
                }
            )


    let invertPuzzle (puzzle:Puzzle) :Puzzle =
        match puzzle with
        | (board, wordCount, words) ->
            let inverseBoard = board |> Array2D.map (fun ltr -> 
                if ltr = EmptyChar then EmptyChar else BoxChar)
            (inverseBoard, wordCount, words)

    // this method is NOT recursive
    // it takes the master list of words and starting puzzle
    // then returns a sequence of puzzles that meet minimum criteria
    let createPuzzles (words:InputWords) (puzzle:Puzzle) :seq<Puzzle> =
        let spaceFixedWords = fixSpacesInWords words |> Seq.toList
        addWordsToPuzzle spaceFixedWords puzzle
        |> Seq.where (fun (_, wordCount, _) -> wordCount = words.Length)
        |> Seq.map (fun (board, wordCount, puzzleWords) -> 
                shrinkPuzzleToSmallest (board, wordCount, puzzleWords |> List.rev))
        |> Seq.map invertPuzzle