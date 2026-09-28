"""Map submitted line references to the fixed revised PDF using token alignment."""
from pathlib import Path
import difflib
import json
import re
import subprocess
from docx import Document
ROOT=Path(__file__).resolve().parents[1]

def read_lines(path: Path) -> list[tuple[int,str]]:
    text=subprocess.check_output(['pdftotext','-layout',str(path),'-'],text=True)
    result=[]
    for line in text.splitlines():
        match=re.match(r'^\s*(\d{1,4})\s+(.+?)\s*$',line)
        if match:result.append((int(match[1]),match[2]))
    return result

def tokens(text: str) -> list[str]:
    return re.findall(r'[a-z0-9]+',text.lower())

def flattened(lines):
    words=[];numbers=[]
    for n,text in lines:
        ts=tokens(text);words.extend(ts);numbers.extend([n]*len(ts))
    return words,numbers

def main() -> None:
    old,on=flattened(read_lines(ROOT/'docs/submission_pdf.pdf'))
    new,nn=flattened(read_lines(ROOT/'outputs/Manuscript_line_reference.pdf'))
    mapping={}
    for op,a,b,c,d in difflib.SequenceMatcher(None,old,new,autojunk=False).get_opcodes():
        if op=='equal':
            for oi,ni in zip(range(a,b),range(c,d)):mapping.setdefault(on[oi],set()).add(nn[ni])
        elif b>a:
            targets=set(nn[c:d]) if d>c else set(nn[max(0,c-1):min(len(nn),c+1)])
            for oi in range(a,b):mapping.setdefault(on[oi],set()).update(targets)
    paragraph_map={}
    for i,p in enumerate(Document(ROOT/'outputs/Manuscript_clean.docx').paragraphs):
        ts=tokens(p.text)
        if not ts:continue
        matched=[]
        # Matching blocks tolerate line-end hyphenation and rendered field spacing.
        for m in difflib.SequenceMatcher(None,ts,new,autojunk=False).get_matching_blocks():
            if m.size>=min(6,len(ts)):
                matched.extend(nn[m.b:m.b+m.size])
        if matched:paragraph_map[str(i)]=[min(matched),max(matched)]
    output={'basis':'Manuscript_line_reference.pdf; LibreOffice rendering. Word may repaginate.',
            'original_to_revised':{str(k):[min(v),max(v)] for k,v in mapping.items() if v},
            'paragraphs':paragraph_map}
    (ROOT/'outputs/line_map.json').write_text(json.dumps(output,indent=2))
    print('Mapped',len(mapping),'original numbered lines and',len(paragraph_map),'paragraphs')

if __name__=='__main__':main()
