# Elm Markdown Parser

**Word of caution: this is still heavily WIP**

Following spec defined at: [https://www.markdownguide.org/basic-syntax/](https://www.markdownguide.org/basic-syntax/)

## Motivation

- Learn how to implement parsers with Elm
- Contribute to the community
- Need something that parses markdown but doesn't fail... Erm much
- And is configurable
- Could be fun
- Or convince me that I should be a farmer

High hopes that I'll actually finish this one :metal:


## Road so far

**Can parse headers**

```
# H1
## H2
### H3
#### H4
##### H5
###### H6

And these kind of H1's
======================

And these kind of H2's
----------------------
```

**Can parse inline content**

```
Parse **bold**, _italic_, and even ~~strikethrough~~ content!
```


## TODO

- Block parsers
    - [x] Headings
        - [ ] Refactor headings parsers
    - [ ] Lists (orderd & unordered)
    - [ ] Blockquotes
    - [ ] Code blocks
- Inline parsers
    - [x] Bold / italic / strikethrough
    - [x] Nested bold / italic / strikethrough
    - [ ] Line breaks
    - [ ] Code
    - [ ] Links
    - [ ] Images
- Other
    - [ ] Rendering

