"""Generate clean and tracked manuscript copies from the immutable source."""
import json
import sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'src'))
from docx_revision import RevisionDocument

def main() -> None:
    """Apply checked revisions and attach traceable review comments."""
    edits=json.loads((ROOT/'outputs/planned_edits.json').read_text())
    document=RevisionDocument(ROOT/'docs/Manuscript.docx')
    for edit in edits:
        document.replace(edit['paragraph'],edit['old'],edit['new'],edit['comments'],edit['reason'])
    for index in sorted({e['paragraph'] for e in edits}):
        entries=[e for e in edits if e['paragraph']==index]
        document.add_comment(index,'; '.join(f"Comments {e['comments']}: {e['reason']}" for e in entries))
    for index,number in [(51,4),(58,5)]:
        document.replace_image(index,ROOT/'outputs/figures'/f'figure{number}_revised.png')
        document.add_comment(index,'Comments 7–12 and 17: model values updated using the original figure style, percentage axis, habitat legend and thick 75%/thin 95% intervals. Vector PDF supplied.')
    document.write(ROOT/'outputs/Manuscript_tracked.docx')
    document.write(ROOT/'outputs/Manuscript_clean.docx',clean=True)
    (ROOT/'outputs/change_log.json').write_text(json.dumps(document.log,indent=2,ensure_ascii=False))
    print(f'Wrote {len(edits)} text edits and two tracked figure replacements.')

if __name__=='__main__':main()
