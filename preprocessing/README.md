# 数据预处理

这个文件夹集中保存问卷数据预处理程序及正式输出。原始个人回答保存在项目根目录的 `demodata/`，问卷程序 `index.html` 仍保存在项目根目录。

## 运行顺序

在项目根目录依次运行：

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ".\preprocessing\01_merge_raw_data.R"
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ".\preprocessing\02_prepare_analysis_data.R"
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ".\preprocessing\03_compute_scale_scores.R"
```

三个程序也可以从其他工作目录运行。默认输入和输出以程序所在位置为基准。

## 程序和输出

### 01 合并原始数据

- 程序：`01_merge_raw_data.R`
- 默认输入：`../demodata/*.csv`
- 输出：`merged_raw_data.csv`
- 功能：把一人一个CSV合并为一人一行，并把随机呈现的PANAS项目恢复为统一题号顺序。

### 02 清理和预处理

- 程序：`02_prepare_analysis_data.R`
- 输入：`merged_raw_data.csv`
- 主输出：`prepared_analysis_data.csv`
- 排除记录：`preprocessing_exclusions.csv`
- 编码说明：`preprocessing_codebook.csv`
- 功能：检查取值范围和题号、排除注意力检查不通过者、保留日语人口统计标签并追加分析编码、追加确认过的逆转题变量。

### 03 计算量表得分

- 程序：`03_compute_scale_scores.R`
- 输入：`prepared_analysis_data.csv`
- 完整输出：`analysis_with_scale_scores.csv`
- 精简输出：`scale_scores.csv`
- 计分说明：`scale_score_codebook.csv`
- 功能：按照各量表和下位尺度计算38个得分。分析数据保留完整小数精度；论文表格中的小数位在之后的记述统计程序中设置。

## 当前正式数据数量

- 合并前个人回答文件：301件
- 注意力检查排除：1件
- 可用于分析：300件

需要项目级数据时使用 `analysis_with_scale_scores.csv`。只需要人口统计变量和量表得分时使用 `scale_scores.csv`。

不要手工修改正式输出CSV。原始数据增加或计分规则改变时，应从01开始依次重新运行。
