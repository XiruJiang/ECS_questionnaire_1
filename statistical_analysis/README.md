# 统计分析程序

本文件夹用于保存毕业论文统计分析程序和结果。输入数据来自 `../preprocessing/03_analysis_with_scale_scores.csv`，程序不会改写预处理数据。

## 与原论文的对应关系

参考论文：Van der Kaap-Deeder et al. (2023), *Emotion crafting: Individuals as agents of their positive emotional experiences*, https://doi.org/10.1007/s11031-023-10035-0

原论文依次进行了ECS因子结构检验、信度分析、相关分析、背景属性MANCOVA、主中介SEM及7种情绪版敏感性分析。本项目按相同逻辑建立程序，但存在以下研究设计差异：

- 当前问卷没有地区变量，因此MANCOVA和SEM无法控制location。
- 当前问卷使用K6测量心理困扰，代替原论文的HSCL-25内化症状。
- 当前问卷使用EAC感情处理8项，代替原论文的ERI integrative emotion regulation 6项。两者概念相关但不是同一量表，解释时不得写成完全复现。
- SEM把ECR-GO依恋不安与亲密性回避放在EC Awareness之前，是本毕业论文追加的理论扩展；原论文没有检验这两条上游路径。
- 当前PANAS为1–6件法，原论文使用1–5件法；分析使用当前问卷的实际计分。
- 模拟数据的分析结果只用于检查程序，不构成研究结论。正式调查完成后必须重新运行全部程序。

## 程序顺序

1. `01_descriptive_statistics.R`：样本流程、人口统计频数、量表得分的M、SD、中位数、范围、偏度与峰度。
2. `02_reliability_analysis.R`：38个量表/下位尺度的Cronbach's alpha、标准化alpha、McDonald's omega total和修正项目—总分相关。二项目下位尺度不估计omega。
3. `03_correlation_analysis.R`：ECS一般版和7种情绪版与情绪调节、幸福感、心理困扰及依恋指标的Pearson相关，同时输出原始p值与BH-FDR。
4. `04_mancova.R`：包含三个模型：背景属性对5个心理结果的MANCOVA、对ECS Awareness/Action的探索性MANCOVA，以及本毕业论文追加的对ECR-GO依恋不安/亲密性回避的探索性MANCOVA。性别仅比较男性/女性；年龄、教育和收入作为协变量；婚姻状况作为固定因子。
5a. `05_efa_ecs.R`：按照原论文先对ECS一般版实施EFA。对当前最终版10项请求1–4因子，使用MLR和Geomin斜交旋转；重点比较1因子与2因子，同时输出交叉负荷、KMO与Bartlett补充诊断。原论文最初对12项实施EFA并据交叉负荷删除2项，当前问卷已不包含被删除的2项，因此本分析是对最终10项结构的探索，不是对原始项目筛选过程的完全复现。
5b. `05_cfa_ecs.R`：ECS一般版和7种情绪版分别比较一因子与Awareness/Action二因子模型。估计方法为MLR，并按原论文设置第5与第8项残差相关。
6. `06_sem_primary_model.R`：潜变量主模型。把ECR-GO的依恋不安与亲密性回避作为两个相关的上游潜变量，检验ECR-GO → Awareness → Action → Positive Affect → Life satisfaction/Vitality/Eudaimonic well-being/Distress；第二模型加入其他情绪调节指标。ECR-GO两个下位尺度分别使用3个平衡题包，连续间接效应默认1000次bootstrap。
7. `07_sem_emotion_specific_sensitivity.R`：把一般版Awareness/Action依次替换为7种情绪版得分，同时保留ECR-GO依恋不安与亲密性回避在路径最上游。仿照原论文的感情别敏感性分析，统一检验Awareness→Positive Affect以及Awareness/Action→4个结果变量的直接路径，并另行输出显著追加路径表。
8. `08_generate_sem_figures.R`：按照原论文图1–3的功能生成论文插图。图1为追加ECR后的理论模型；图2显示一般版主SEM的标准化系数（斜线前后分别为模型1/模型2）；图3把7种具体情绪版合并到一个结构图中，以a–g区分感情。主要路径用 `/` 分隔7个模型的系数；追加直接路径的系数右上角字母表示对应的情绪；Positive Affect之后的路径及结果变量间残差相关用 `;` 分隔7个模型中的最小值和最大值。蓝色虚线表示当前数据中显著的追加直接路径，灰色点线表示原论文检验过但当前数据不显著（NS）的路径。每图同时输出180 dpi PNG和矢量PDF，正式数据分析后需重新生成。
9. `09_generate_paper_tables.R`：将表2、表3、表4、表5所需结果整理为可直接复制到Excel的专用CSV。表2合并EFA、CFA和主要SEM拟合指标；表3合并一般版ECS的标准化因子负荷量、R²、题目M和SD；表4输出M、SD及带显著性星号的下三角相关矩阵，并追加本研究使用的ECR-GO两个下位尺度；表5输出主要SEM模型的间接效果及95%置信区间，并追加ECR-GO相关间接路径；其中`ci_95_display`列按原论文格式生成可直接复制的`[.234, .603]`形式。表5优先读取1000次以上的percentile bootstrap结果；若bootstrap结果不存在，则明确标记为仅供程序检查的delta-method暂定值，不得用于正式报告。

## 运行方法

首次使用时检查或安装依赖：

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ".\statistical_analysis\00_install_required_packages.R"
```

正式分析（SEM使用1000次bootstrap）：

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ".\statistical_analysis\run_all_analyses.R"
```

仅用于快速检查程序：

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ".\statistical_analysis\run_all_analyses.R" --quick
```

也可以单独运行任一编号程序。所有结果保存在 `results/` 的对应子文件夹。

## 报告精度

分析过程使用未取整数据。输出表通常将M、SD和模型系数显示至2–3位小数，p值显示至3–4位；不得在相关、回归、CFA或SEM之前先对量表得分四舍五入。

## 解释顺序

- 首先确认数据质量、样本流程和记述统计。
- 其次确认ECS二因子结构和下位尺度信度。
- 然后解释ECS与其他变量的相关关系。
- 主假设使用SEM检验；MANCOVA用于背景属性差异的补充说明。
- 7种情绪版模型作为敏感性分析，不要求所有结果都与一般版完全一致。
