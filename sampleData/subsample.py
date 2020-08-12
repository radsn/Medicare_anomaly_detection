import pandas as pd
import re
from pathlib import Path


# yearFile valid over set [12,17]
yearFile = lambda y: Path("data",f"Medicare_Provider_Util_Payment_PUF_CY20{y}.txt")

# find filenames to keep to standardize imports
toKeep = lambda c: re.search("(stdev)|(standard)",c.lower()) is None


def sampleData(out=Path("data","sample.csv"),sampleSize=100000,start = 12, end = 17,fileGenerator = yearFile, keepCondition=toKeep):
    for y in range(start,end+1):
        df = pd.read_csv(fileGenerator(y),sep="\t",header=0,skiprows=[1],usecols=keepCondition)
        df.columns = df.columns.str.lower()
        year = 2000 + y
        df["year"] = 2000 + y
        if y == start:
            df.sample(sampleSize).to_csv(out)
        else:
            df.sample(sampleSize).to_csv(out,mode='a', header=False)
        print(f"Finished year {year}")




if __name__ == "__main__":
    sampleData(out=Path("data","demo.csv"))
