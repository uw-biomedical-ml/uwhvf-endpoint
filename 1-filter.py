#!/usr/bin/env python

import json
from tqdm import tqdm
import numpy as np
import random

random.seed(137)

with open("alldata.json") as fin:
    alldata = json.load(fin)
alldata = alldata["data"]

found = [0,0]
ptgrps = {}

with open("fields.tsv", "w") as fout2:
    header = []
    for i in range(1,55):
        header.append(f"p{i}")
    header += ["age", "eye", "gender", "status", "grp", "foldid", "time", "startmd"]
    fout2.write("%s\n" % "\t".join(header))
    with open("ptlvl.tsv", "w") as fout:
        fout.write("ptid\teye\tgender\tyear\tstartmd\tage\tevent\ttime\tgrp\n")
        for ptid in tqdm(alldata.keys()):
            ld = None
            grp = "train"
            if random.random() < 0.5:
                grp = "test"
            foldid = random.randint(1,10)
            seq = {}
            gender = alldata[ptid]["gender"]
            year = alldata[ptid]["year"]
            for eye in ("R", "L"):
                if not eye in alldata[ptid]:
                    continue
                firstage = None
                age = None
                last = None
                for dat in alldata[ptid][eye]:
                    age = dat["age"]
                    if firstage == None:
                        firstage = age
                        startmd = np.mean(dat["td_seq"])
                        seq[0.0] = dat["td_seq"]
                        last = age
                    if age - last > 2.25:
                        break
                    seq[age - firstage] = np.array(dat["td_seq"])
                    last = age
                event = 0
                ld = seq[0.0]
                last = np.zeros(ld.shape, dtype=np.uint8)
                for d in sorted(seq.keys()):
                    if d == 0.0:
                        continue
                    diff = seq[d] - ld
                    curdiff = (diff <= -7.0 ).astype(np.uint8)
                    accum = last + curdiff
                    db7 = np.sum(accum == 2)
                    last = curdiff
                    if db7 >= 5:
                        event = 1
                        time = d
                        break
                    time = d
                if time == 0:
                    continue
                if time >= 5.0:
                    found[event] += 1
                out = (ptid, eye, gender, year, startmd, firstage, event, time, grp)
                fout.write("%s\n" % "\t".join(map(str,out)))
                eyebin = 0
                if eye == "L":
                    eyebin = 1
                genderbin = 2
                if gender == "M":
                    genderbin = 0
                if gender == "F":
                    genderbin = 1

                fout2.write("%s\t%f\t%d\t%d\t%d\t%s\t%d\t%f\t%f\n" % ("\t".join(map(str, ld)), firstage, eyebin, genderbin, event, grp,foldid, time, startmd))
    print(found)
