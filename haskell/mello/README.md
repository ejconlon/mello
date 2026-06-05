# mello

No-fuss syntax with s-expressions

## Parsing Diagnostics

`Mello.Parse` exposes two parsing entry points:

- `parseSexp :: Text -> Either (Err Void) LocSexp` preserves the original Looksee error API.
- `parseSexpDetailed :: Text -> Either ParseErr LocSexp` adds Mello-domain diagnostics before falling back to Looksee errors.

`ParseErr` classifies delimiter failures with source locations:

- `ParseErrUnclosedList Brace LocSpan`
- `ParseErrUnexpectedClose Brace LocSpan`
- `ParseErrMismatchedBrace Brace LocSpan Brace LocSpan`
- `ParseErrLooksee LocSpan (Err Void)`

`ParseErrLooksee` intentionally keeps the raw `Looksee.Err` while also carrying its span converted to `LocSpan`. This lets clients either render with Looksee or use Mello line/column locations without traversing Looksee internals.

For interactive use, `parseSexpI` prints Looksee parse errors and `parseSexpDetailedI` prints detailed delimiter errors or the underlying Looksee parse error.

## Delimiter Recognition

`Mello.Recognize` provides a lightweight delimiter recognizer used by `parseSexpDetailed`:

- `recognizeSexpText :: Text -> Either RecogErr ()`
- `sexpRecognizer :: Fold Char (Either RecogErr ())`

The recognizer checks delimiter balance while respecting strings, chars, comments, and escapes. It does not parse s-expressions; it only reports offset-based delimiter errors:

- `RecogErrUnclosedList Brace openOffset eofOffset`
- `RecogErrUnexpectedClose Brace closeOffset`
- `RecogErrMismatchedBrace expected openOffset actual closeOffset`
