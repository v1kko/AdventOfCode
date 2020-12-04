def parse_to_doc(string):
  doc = {}
  fields = string.split()
  for field in fields:
    key,val = field.split(":")
    doc[key] = val
  return doc

required_fields = ['byr','iyr','eyr','hgt','hcl','ecl','pid'] 

def is_passport(doc):
  for req in required_fields:
    if not req in doc:
      return False
  return True

with open("input", "r") as f:
  rawdocs = f.read().split('\n\n')
  docs = [parse_to_doc(rawdoc) for rawdoc in rawdocs]
  print(len([doc for doc in docs if is_passport(doc)]))
  
