#!/usr/bin/env python

import argparse
import os


def is_valid_file(file, parser):
    """
    Checks to determine if the specified file exists

    :param file: A string containing a filepath
    :param parser: An argparse.ArgumentParser object
    :return: file, if the file exists
    :raises: parser.error, if the file does not exists
    """

    if os.path.exists(file):
        return file
    else:
        raise parser.error("Unable to locate %s: No such file or directory" % file)


def get_args():
    """
    Processes command line arguments

    :return: A namespace containing command line arguments
    """
    parser = argparse.ArgumentParser(description="Converts one or more NCBI FLAT files into a single FASTA file")
    parser.add_argument("-i", "--input", metavar="FLAT", required=True, nargs="+",
                        type=lambda x: is_valid_file(x, parser), help="Input FLAT file(s)")
    parser.add_argument("-o", "--output", metavar="FASTA", required=True, help="Output merged FASTA file")
    return parser.parse_args()


def main(args=None):
    if args is None:
        args = get_args()

    # Process each FLAT file
    with open(args.output, "w") as o:
        for file in args.input:
            with open(file) as f:

                sequence = ""
                taxonomy = ""
                organism = ""
                accession = ""

                while True:
                    line = f.readline()
                    if line == "":  # There are no more lines in this file
                        break
                    line = line.strip("\n").strip("\n")  # Remove newline

                    # Parse the required attributes from the FLAT file
                    if line.startswith("ORIGIN"):  # Sequence

                        while True:
                            line = f.readline()
                            line = line.strip("\r").strip("\n")  # Remove newline
                            if line == "//":  # The // is a control character to end the FLAT entry
                                # Format all the file features into a FASTA entry
                                # The format is >taxonomy; Organism; Accession
                                fasta_name = ">Root;" + taxonomy + organism.replace(" ", "_") + ";" + accession
                                o.write(fasta_name)
                                o.write(os.linesep)

                                # Sequence
                                o.write(sequence)
                                o.write(os.linesep)

                                # Reset variables
                                sequence = ""
                                taxonomy = ""
                                organism = ""
                                accession = ""
                                break

                            # Remove all spaces and numbers
                            line = line.replace(" ", "")
                            line = "".join(list(x for x in line if not x.isdigit()))
                            sequence += line
                    elif line.startswith("SOURCE"):
                        while True:
                            line = f.readline()
                            line = line.strip("\r").strip("\n")  # Remove newline
                            if line[0] != " ":
                                break

                            if "ORGANISM" in line:
                                organism = " ".join(line.split()[1:])
                            else:  # Taxonomy information
                                taxonomy += line.replace(" ", "")
                        # Replace the last period after the taxonomy info with a semicolon
                        taxonomy = taxonomy.replace(".", ";")
                    elif line.startswith("ACCESSION"):
                        accession = line.split()[1]


if __name__ == "__main__":
    main()
