#!/usr/bin/env python

buckets = {}
with open("simultation.csv") as fin:
    header = None
    for l in fin:
        arr = l.strip().split(",")
        if header == None:
            header = arr
            continue
        (ss, hr, yr, p) = arr
        p = float(p)
        hr = round(float(arr[1]),2)
        if hr == 0.0 or hr == 1.0:
            continue
        key = ",".join(map(str, (ss,yr,hr)))
        if not key in buckets:
            buckets[key] = [0, 0]
        buckets[key][1] += 1
        if p < 0.05:
            buckets[key][0] += 1

with open("power.csv", "w") as fout:
    fout.write("sample.size,length,hazard.ratio,power,simn\n")
    for key in sorted(buckets.keys()):
        power = round(100.0 * buckets[key][0] / buckets[key][1], 2)
        fout.write("%s,%0.2f,%d\n" % (key, power, buckets[key][1]))



