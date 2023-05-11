#sort fasta containing ITS2, rbcL, and 18S into separate files containing just one marker each

from datetime import date
import glob
from Bio import SeqIO
import re

# input fasta paths (can be multiple fastas with regex if desired, or just a single file)
all_fastas = glob.glob("/home/cengstro/ownCloud/proj/single_cell/data/seq/genbank/all_markers_for_tree.fasta")
# print(all_fastas)

# define outpaths
its2_out_path = "/home/cengstro/ownCloud/proj/single_cell/data/seq/its2.fasta"
x18s_out_path = "/home/cengstro/ownCloud/proj/single_cell/data/seq/x18s.fasta"
rbcl_out_path = "/home/cengstro/ownCloud/proj/single_cell/data/seq/rbcL.fasta"

its2_out_handle=open(its2_out_path,'w')
x18s_out_handle=open(x18s_out_path,'w')
rbcl_out_handle=open(rbcl_out_path,'w')

# loop through the fastas and sort the entries
for fasta in all_fastas:
    parsed_fasta = SeqIO.parse(open(fasta),'fasta')
    print("opening: " + fasta)
    # open the fasta file
    for record in parsed_fasta:
        my_seq = str(record.seq) # the sequence data
        my_header = str(record.description) # the fasta header
        # print(my_header)
        # search for regex in fasta headers, and write the header and seq to that handle

        # ITS2
        if re.search('ITS2|internal', my_header, re.IGNORECASE): 
            print("writing ITS2: " + my_header)
            its2_out_handle.write(">" + my_header + "\n") # write header
            its2_out_handle.write(my_seq + "\n") # write seq
        # 18S
        if re.search('18S', my_header, re.IGNORECASE): # don't use elif, since a single header might contain both its2 and 18s
            print("writing 18S: " + my_header, re.IGNORECASE)
            x18s_out_handle.write(">" + my_header + "\n") 
            x18s_out_handle.write(my_seq + "\n") 
        # rbcL
        if re.search('rbcL|ribulose', my_header, re.IGNORECASE):
            print("writing rbcL: " + my_header)
            rbcl_out_handle.write(">" + my_header + "\n") 
            rbcl_out_handle.write(my_seq + "\n") 
