#!/usr/bin/env python

import json
from tqdm import tqdm
import numpy as np


def calcMD(hvf, td):
    md = np.mean(td)
    return md

with open("alldata.json") as fin:
    alldata = json.load(fin)
alldata = alldata["data"]

found = [0,0]
with open("ptlvl.tsv", "w") as fout:
    fout.write("ptid\teye\tgender\tyear\tstartmd\tstartmdcat\tage\tevent\ttime\n")
    for ptid in tqdm(alldata.keys()):
        ld = None
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
            startmdcat = None
            if startmd > -5:
                startmdcat = 0
            elif startmd <= -5 and startmd > -10:
                startmdcat = 1
            elif startmd <= -10 and startmd > -15:
                startmdcat = 2
            elif startmd <= -15:
                startmdcat = 3
            if time == 0:
                continue
            if time >= 5.0:
                found[event] += 1
            out = (ptid, eye, gender, year, startmd, startmdcat, firstage, event, time)
            fout.write("%s\n" % "\t".join(map(str,out)))
print(found)
