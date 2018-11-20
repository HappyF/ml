## xgboost 学习笔记及应用
### 1 xgboost来历
xgboost的全称是eXtreme Gradient Boosting。它是Gradient Boosting Machine的一个c++实现。它兼具**线性模型**求解器和**树学习**算法。因此，它快速的秘诀在于算法在单机上也可以并行计算的能力。
> Gradient Boosting Machine:

### 2 优点
- 高效
	- xgboost自定义了一个数据矩阵类DMatrix，会在训练开始时进行一遍预处理，从而提高之后每次迭代的效率
	- xgboost借助**OpenMP**，能自动利用单机CPU的多核进行并行计算
- 准确性
- 模型的交互性

### 3 推导


### 4 [实现](https://segmentfault.com/a/1190000004421821)

>   XGBoost仅适用于数值型向量。需要使用中区分数据类型。如果是名义，比如“一年级”、“二年级”之类的，需要变成哑变量，然后进行后续的处理。

稀疏矩阵是一个大多数值为零的矩阵。
### 5  实际应用案例
- 测井曲线对油田的预测 [github]
	
- 促销预测 [github]
数据来源于《预测分析中的建模技术》中第二章 
	- step1 探索性数据分析
	- step2 构建响应模型，并做数据的稀疏化 
	- step3 模型的评价 
	- step4 预测 
	- step5 评估
	- 大规模并行化实现？

- 网络案例