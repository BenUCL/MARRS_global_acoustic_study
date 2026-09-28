"""Extract the complete scientific review text into a stable numbered register."""
import json
import re
import subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]

def pdf_text(name: str) -> str:
    """Extract PDF text, removing repeating headers and contact information."""
    text=subprocess.check_output(['pdftotext','-layout',str(ROOT/'docs'/name),'-'],text=True)
    lines=[]
    for line in text.splitlines():
        if any(x in line for x in ['https://mc.manuscriptcentral.com','9/20/26,','Confidential Review']):
            continue
        lines.append(line.strip())
    return '\n'.join(lines)

def clean(text: str) -> str:
    return re.sub(r'\s+',' ',text).strip()

def main() -> None:
    a=pdf_text('comments1.pdf'); b=pdf_text('comments2.pdf')
    r1=a.split('REVIEWER REPORT FOR THE AUTHORS')[1].split('Reviewer: 2')[0]
    r1=r1.split('Excellent work and I look forward')[0]
    chunks=re.split(r'(?m)\n\n(?=L\d|I loved|Fig 4)',r1)
    entries=[]
    for text in chunks[1:]:
        entries.append({'id':len(entries)+1,'reviewer':'Reviewer 1','comment':clean(text)})
    if len(entries)!=9: raise ValueError(f'Expected 9 reviewer 1 points, got {len(entries)}')
    content=b.split('Major comments')[1].replace('Minor comments','')
    chunks=re.split(r'(?m)(?=^Lines? \d)',content)
    for text in chunks:
        if clean(text): entries.append({'id':len(entries)+1,'reviewer':'Reviewer 2','comment':clean(text)})
    assert len(entries)==18
    ae=a.split('COMMENTS FOR THE AUTHORS - MANDATORY:')[1].split('***************************************************')[0]
    entries.append({'id':19,'reviewer':'Associate Editor','comment':clean(ae)})
    editor=a.split('Overall, most comments')[1].split('In your revision, please')[0]
    bits=editor.split('In addition to addressing')
    entries.append({'id':20,'reviewer':'Editor','comment':clean('Overall, most comments'+bits[0])})
    entries.append({'id':21,'reviewer':'Editor','comment':clean('In addition to addressing'+bits[1])})
    # Retain substantive instructions, omit portal navigation, advertising and contacts.
    instructions=[
      'Please note that your amended manuscript should still abide by our double-anonymous peer review model and your point-by-point response should not contain author names or identifying information.',
      'A clean version of the revised manuscript should be uploaded as the Main Document and a version with tracked changes should also uploaded as a separate File for Editors to allow editors to easily see the changes made.',
      'The first point sets the wider context for your work and emphasises its applied significance and novelty. The final point is the most important in maximising the impact of the paper. It should synthesise the paper’s key messages and should be generic, seminal and accessible to non-specialists, and must carry one of the following subheadings: “Synthesis and applications” for articles that identify recommendations for management practices; “Policy implications” for articles that are less directly tied to on-the-ground management and include discussion on conservation implications or links to policy.',
      'Before submitting your revision, please upload a graphical abstract - a high-resolution, eye-catching image related to the study and its relevance. You do not need to supply an additional legend because graphical abstracts appear alongside the final paragraph of abstracts in the table of contents for the journal.',
      'In accordance with our Data archiving policy, data (or, for theoretical papers, mathematical and computer models) supporting the results in papers published in Journal of Applied Ecology will need to be archived in an appropriate public archive. Please ensure that your revised manuscript includes a data availability statement.',
      'The revision should not exceed 7000 words for Research Articles, 8000 for Reviews and 4000 for all other article types. This includes all text, table legends and figure legends. Reference lists and Supporting Information are not included in the word count.',
      'Please use Supporting Information to publish important ancillary information. It should only include information and analyses of clear and direct relevance to the message conveyed in your manuscript and it should not be used as a repository for source data.',
      'Please return your revision within three months (90 days) of the date of this message. If you need longer, please let us know so we can update our system accordingly.',
      'Optional: authors may provide one additional abstract in their own language or a language relevant to the research. Place it below the English abstract; it is published online and is not copyedited.',
      'Ensure that all co-authors have the necessary rights to grant the chosen publication licence, including any restrictions arising from funder policies.'
    ]
    entries.append({'id':22,'reviewer':'Resubmission instructions','comment':'Administrative requirements from the decision letter (navigation and optional language-editing advertising omitted).','parts':instructions})
    # Exact sentence boundaries preserve the reviewer wording and make requests traceable.
    splits={11:['Additionally,'],13:['Hard coral cover','If other models'],15:['There is no characterisation'],16:['I also note','Was this sonotype'],19:['In particular,','My other major concern','This is particularly relevant','Otherwise,']}
    for e in entries:
        if e['id'] in splits:
            pattern='|'.join('(?='+re.escape(x)+')' for x in splits[e['id']])
            e['parts']=[clean(x) for x in re.split(pattern,e['comment']) if clean(x)]
    payload={'preambles':{'Reviewer 1':clean(r1.split('L19:')[0]),
                         'Reviewer 2':clean(b.split('Major comments')[0]),
                         'Reviewer 2 covering note':'I greatly enjoyed reading this manuscript, it’s a great contribution to the fields of PAM and coral reef restoration. I hope that you find my comments helpful and constructive.'},'comments':entries}
    (ROOT/'outputs/comment_register.json').write_text(json.dumps(payload,indent=2,ensure_ascii=False))
    print([(e['id'],e['comment'][:70]) for e in entries])

if __name__=='__main__': main()
