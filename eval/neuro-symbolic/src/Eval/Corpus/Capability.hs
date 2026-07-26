module Eval.Corpus.Capability (
    capabilityTasks,
) where

import Eval.Task (Grader (..), Task (..))

capabilityTasks :: [Task]
capabilityTasks =
    [ imageInfoTask
    , locationCodeTask
    , unescapeTask
    , configValueTask
    , qrSizeTask
    , wordFreqTask
    ]

imageInfoTask :: Task
imageInfoTask =
    Task
        "imageInfo"
        "Here is a small colour picture, encoded as a block of base64 text: \
        \\"iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAIAAADZSiLoAAAAJUlEQVR4nGNgYGBIYdA4wRAA\
        \pDVSUgJOpFQwMJwISDlRceLEAgBp0gnZl566IAAAAABJRU5ErkJggg==\". \
        \Read the picture out of that text and tell me two things: how big it is \
        \(its width and height in pixels), and the colour of the pixel in column \
        \1, row 1 (both counting from 0), given as its red, green and blue \
        \amounts. Define a pure binding \
        \`imageInfo :: ((Int, Int), (Int, Int, Int))` whose first part is the \
        \(width, height) and whose second part is that pixel's (red, green, blue)."
        (ByValue "imageInfo == ((3, 3), (100, 100, 80))")

locationCodeTask :: Task
locationCodeTask =
    Task
        "locationCode"
        "Turn a latitude and longitude into a short piece of text that stands for \
        \that spot on Earth, where two places that are near each other share the \
        \same starting letters and a longer code pins down a smaller area. Define \
        \a pure binding `locationCode :: Double -> Double -> Int -> String` taking \
        \the latitude, the longitude, and how many characters of detail to \
        \produce. As a worked example, asking for 5 characters of detail at \
        \latitude 37.7749, longitude -122.4194 must give exactly \"9q8yy\", and \
        \asking for 6 characters at the same spot must start with those same 5 \
        \characters."
        ( ByValue
            "locationCode 37.7749 (-122.4194) 5 == \"9q8yy\" \
            \&& take 5 (locationCode 37.7749 (-122.4194) 6) == locationCode 37.7749 (-122.4194) 5"
        )

unescapeTask :: Task
unescapeTask =
    Task
        "unescape"
        "Some text has little &-codes standing in for characters that are awkward \
        \to type, such as &lt; for a less-than sign, &amp; for an ampersand, \
        \&copy; for a copyright sign, &reg; for a registered-trademark sign, \
        \&nbsp; for a no-break space, and &mdash; for a long dash. There are also \
        \numeric forms: &#169; means the character with number 169, and \
        \&#x2764; means the character with hexadecimal number 2764. Replace every \
        \such code with the real character it stands for and leave the rest of \
        \the text untouched. Define a pure binding \
        \`unescape :: String -> String`. As worked examples, \
        \`unescape \"5 &lt; 10 &amp; 10 &gt; 5\"` is \"5 < 10 & 10 > 5\"; the codes \
        \&copy; and &reg; and &nbsp; become the characters numbered 169, 174 and \
        \160; and `unescape \"&#169;&#x2764;\"` is the two characters numbered 169 \
        \and 10084."
        ( ByValue
            "unescape \"5 &lt; 10 &amp; 10 &gt; 5\" == \"5 < 10 & 10 > 5\" \
            \&& unescape \"&copy; 2024 &reg; A&nbsp;B\" == \"\\169 2024 \\174 A\\160B\" \
            \&& unescape \"&#169;&#x2764;\" == \"\\169\\10084\""
        )

configValueTask :: Task
configValueTask =
    Task
        "configValue"
        "Read a single value out of a block of indented settings text by \
        \following a dotted path of nested key names. Indentation shows nesting: \
        \a key indented under another belongs to it. Define a pure binding \
        \`configValue :: String -> String -> Maybe String` where the first \
        \argument is the settings text and the second is a path like \
        \\"database.host\". Return Just the leaf value as text (a number such as \
        \5432 comes back as \"5432\"; a yes/no value comes back as \"true\" or \
        \\"false\"), or Nothing if the path is not there or the text cannot be \
        \read. As a worked example, given the text\n\n\
        \    database:\n\
        \      host: localhost\n\
        \      port: 5432\n\
        \      enabled: true\n\
        \    name: prod\n\n\
        \the path \"database.host\" gives Just \"localhost\", \"database.port\" \
        \gives Just \"5432\", \"database.enabled\" gives Just \"true\", \"name\" \
        \gives Just \"prod\", and \"database.missing\" gives Nothing."
        ( ByValue
            "(let doc = \"database:\\n  host: localhost\\n  port: 5432\\n  enabled: true\\nname: prod\\n\" \
            \ in configValue doc \"database.host\" == Just \"localhost\" \
            \ && configValue doc \"database.port\" == Just \"5432\" \
            \ && configValue doc \"database.enabled\" == Just \"true\" \
            \ && configValue doc \"name\" == Just \"prod\" \
            \ && configValue doc \"database.missing\" == Nothing)"
        )

qrSizeTask :: Task
qrSizeTask =
    Task
        "qrSize"
        "Turn the text \"hello world\" into the kind of square grid of tiny black \
        \and white squares that a phone camera can scan to read the text back. \
        \Tell me how big the grid is. Define a pure binding \
        \`qrSize :: (Int, Int, Bool)` whose three parts are the grid's width in \
        \squares, its height in squares, and whether it is a perfect square (same \
        \width and height, and not empty). The grid for \"hello world\" is the \
        \same width as height and at least 21 squares on a side."
        (ByValue "(let (w,h,sq) = qrSize in sq && w == h && w >= 21)")

wordFreqTask :: Task
wordFreqTask =
    Task
        "wordFreq"
        "Count how many times each word appears in a piece of text. Words are \
        \separated by spaces. Define a pure binding \
        \`wordFreq :: String -> [(String, Int)]` returning each distinct word \
        \paired with the number of times it occurs. As a worked example, in \
        \\"the cat the dog the\" the word \"the\" appears 3 times, \"cat\" once \
        \and \"dog\" once, and the counts add up to 5."
        ( ByValue
            "(let r = wordFreq \"the cat the dog the\" \
            \ in lookup \"the\" r == Just 3 && lookup \"cat\" r == Just 1 \
            \ && lookup \"dog\" r == Just 1 && sum (map snd r) == 5)"
        )
