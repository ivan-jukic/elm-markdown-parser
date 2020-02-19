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


## TODO

- Parsers
    - [x] Headings
    - [x] Bold / italic / strikethrough
    - [ ] Nested bold / italic / strikethrough
    - [ ] Inline code
    - [ ] Links
    - [ ] Images
    - [ ] Lists (orderd & unordered)
    - [ ] Blockquotes
    - [ ] Code blocks
- Other
    - [ ] Rendering

Plus also, tests for all!
