# Business Analytics - 교내 공모전 최우수상 (총장상) 🏆
Repository for Final Project of Business Analytics(MEC40091) <br/>
Handong University, 2021 Fall, Prof. Juhee Oh 

<img src="https://img.shields.io/badge/R-green?style=flat&logo=R&logoColor=276DC3"/> <img src="https://img.shields.io/badge/RStudio-red?style=flat&logo=RStudio&logoColor=75AADB"/> <img src="https://img.shields.io/badge/Excel-yellow?style=flat&logo=Microsoft Excel&logoColor=217346"/> 


- [Final PPT](https://drive.google.com/file/d/1iTfea6UMdtbrSeKohPs3ZoPZV-anAEC1/view)
- [Final Report](https://drive.google.com/file/d/1Set8N-hyo8-xTqoiPpi5OlESP0k594K1/view)

---

### 🧭  Background
- 기업의 도산 (Default)을 예측하고, 대응하는 것의 중요성
  - 기업의 현금 흐름이 충분하지 않은 경우에, 이자 등을 감당하는 것이 어려워짐
  - 개인 역시 생산성이 떨어지며, 정신적 스트레스로 인한 추가적인 피해 우려 발생
  - 궁극적으로, 거버넌스 관점에서의 매커니즘 정립이 필요함
- 개인의 재무적 상황을 기반으로 하여 상환 능력이 있는 개인들을 구별하는 것이 필요함
- 유사한 고객군을 기반으로 하여 신규 고객에 대해 적절한 대응을 추론 할 수 있음

### 🎁  Service
- 개인의 재무 상황에 대한 약 11개의 Kaggle 데이터 활용
- 개인의 Default를 예측할 수 있는 모델링
- 유사한 재무 상황을 가진 고객군을 군집화

### 🖇  Process
- 데이터 수집: Kaggle 데이터 활용 (150,000 Observations, 11 Variables)
- 데이터 전처리 및 스키마 통일: NA 및 이상값 처리, 데이터 정규화, EDA, 시각화 등
- Default 확률 예측 모델 만들기: Logistic Regression을 활용하여 모델링 수행
- 유사한 고객군에 맞는 조언 제공: K-means 알고리즘을 통한 고객 Clustering

### 📌  Conclusion
- 미시적 및 거시적 관점에서 결과물 활용 가능
  - 개인의 신용도를 파악하여 그에 맞는 개인화 된 서비스 제공 가능
  - 대출자들의 부도로 인한 잠재적 위험을 감소 시킬 수 있음
  - 개인마다 상황에 맞게 적절한 금리를 제공할 수 있음
  
### 👨‍👩‍👧‍👦  Role
- 박성찬 (팀장): 기획, 데이터 수집, 데이터 전처리, 보고서, 발표
