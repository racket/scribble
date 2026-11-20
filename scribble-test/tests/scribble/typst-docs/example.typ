#Sheading(depth: 1)[Title]
 <x28part_x22Titlex22x29>


#Sheading(depth: 2)[1\. Section]
 <x28part_x22Sectionx22x29>

This is a top\-level section\.

#Sheading(depth: 3)[1\.1\. Subsection]
 <x28part_x22Subsectionx22x29>

This is a subsection\.

#Sheading(depth: 4)[1\.1\.1\. Subsubsection]
 <x28part_x22Subsubsectionx22x29>
#metadata(none)<x28modx2dpath_x22racketx2fstringx22x29>

This is a subsubsection\.

Here is an itemize:

#list(
[Item 1\.
],
[Item 2\.
])
Here is a hyperlink:

#link("http://racket-lang.org/")[I am a hyperlink to Racket\.]

#link("http://racket-lang.org/")[I am a #strong[Bold] hyperlink to Racket\.]

#link("http://racket-lang.org/")[I am a #strong[Bold] hyperlink to Racket with \[wacky characters\]\(blah blah\)\.]

#emph[Italic]\.
\_Just underlines\_\.

#strong[Bold]\.
\*Just asterisks\.\*

“Dobule quoted”\.
‘Single quoted’\.

This should NOT be ‘code‘ in Markdown\.

Example of vebatim:

#SVerbatim(
  columns: 1,
  table.cell[#Stt[Hi, world\.]], 
  table.cell[#Stt[A \`\`quote\'\'\.]], 
  table.cell[#Stt[Second line\.]], 
  table.cell[#Stt[Last line\.]], 
)

Another example of verbatim, with ticks\/quotes:

#SVerbatim(
  columns: 1,
  table.cell[#Stt[THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS]], 
  table.cell[#Stt[\`\`AS IS\'\' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT]], 
  table.cell[#Stt[LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR]], 
  table.cell[#Stt[A PARTICULAR PURPOSE ARE DISCLAIMED\. IN NO EVENT SHALL THE COPYRIGHT]], 
  table.cell[#Stt[HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,]], 
  table.cell[#Stt[SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES \(INCLUDING, BUT NOT]], 
  table.cell[#Stt[LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES\; LOSS OF USE,]], 
  table.cell[#Stt[DATA, OR PROFITS\; OR BUSINESS INTERRUPTION\) HOWEVER CAUSED AND ON ANY]], 
  table.cell[#Stt[THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT]], 
  table.cell[#Stt[\(INCLUDING NEGLIGENCE OR OTHERWISE\) ARISING IN ANY WAY OUT OF THE USE]], 
  table.cell[#Stt[OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE\.]], 
)

Example of a defmodule:

#defmodule(
  columns: 2,
  table.cell(colspan: 2)[#Stt[ ]#RktPn[\(]#RktSym[#badlink[#RktValLink[require]]]#Stt[ ]#link(label("x28modx2dpath_x22racketx2fstringx22x29"))[#RktModLink[#RktSym[racket\/string]]]#RktPn[\)]], 
)

Example of a defproc:

#SVerticalInset[#Sboxed(
  columns: 1,
  table.cell[#RBackgroundLabel[#RBackgroundLabelInner[procedure
]
]
#RktPn[\(]#metadata(none)<x28def_x28x28lib_x22racketx2fstringx2erktx22x29_makex2dstringx29x29>#RktSym[#badlink[#RktValLink[make\-string]]]#Stt[ ]#RktVar[k]#Stt[ ]#RktOpt[\[]#RktVar[char]#RktOpt[\]]#RktPn[\)]#Stt[ ]→#Stt[ ]#RktSym[#badlink[#RktValLink[string?]]]], 
  table.cell[#Stt[  ]#RktVar[k]#Stt[ ]:#Stt[ ]#RktSym[#badlink[#RktValLink[exact\-nonnegative\-integer?]]]], 
  table.cell[#Stt[  ]#RktVar[char]#Stt[ ]:#Stt[ ]#RktSym[#badlink[#RktValLink[char?]]]#Stt[ ]\=#Stt[ ]#RktVal[\#\\nul]], 
)
]

Returns a new mutable string of length #RktVar[k] where each position in the
string is initialized with the character #RktVar[char]

Blah blah #RktPn[\(]#RktSym[#badlink[#RktValLink[or\/c]]]#Stt[ ]#RktSym[#badlink[#RktValLink[string?]]]#Stt[ ]#RktSym[#badlink[#RktValLink[bytes?]]]#RktPn[\)]\.

Example of Scribble #RktSym[examples]:

Examples:

#SCodeInset[#RktBlk(
  columns: 1,
  table.cell[#Stt[\> ]#RktPn[\(]#RktSym[#badlink[#RktValLink[define]]]#Stt[ ]#RktSym[x]#Stt[ ]#RktVal[0]#RktPn[\)]], 
  table.cell[#Stt[\> ]#RktPn[\(]#RktSym[#badlink[#RktValLink[displayln]]]#Stt[ ]#RktSym[x]#RktPn[\)]], 
  table.cell[#RktOut[0]], 
)
]

Example of Scribble #RktSym[interaction]:

#SCodeInset[#RktBlk(
  columns: 1,
  table.cell[#Stt[\> ]#RktPn[\(]#RktSym[#badlink[#RktValLink[define]]]#Stt[ ]#RktSym[x]#Stt[ ]#RktVal[0]#RktPn[\)]], 
  table.cell[#Stt[\> ]#RktSym[x]], 
  table.cell[#RktRes[0]], 
)
]

#refpara[#refcolumn[#refcontent[Note: This is a note\. Let’s make it long enough that the
markdown output will have to line\-wrap, to make sure the \> mark starts
each line properly\.
]
]
]
