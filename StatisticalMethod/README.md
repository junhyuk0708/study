# Rstudio 프로젝트 설정 가이드

## 1. Rstudio에서 프로젝트 생성
- Rstudio를 연다.
- 상단 메뉴에서 `New Project`를 선택한다.
- 원하는 폴더를 설정하여 프로젝트를 생성한다. ex) EDA

## 2. `.Rprofile` 파일 설정
- 프로젝트 내 `.Rprofile` 파일을 편집한다.
- 아래 코드를 추가한다:
  ```R
  source("swap.R")
  source("eda-funs.R")

**프로젝트를 생성한 경로에 .Rprofile, swap.R, eda-funs.R 파일 넣어두기** <br>
*source 파일명은 본인이 프로젝트 상황에 맞게 설정하기*
1. 자주 쓰는 함수 정리
2. 색깔 지정 ex) lre <- "#A50034" #lightRed
3. 데이터 경로 설정
4. 패키지 설정

## 3. 생성한 프로젝트 Rstudio로 열기
- 'EDA' 파일을 연다.
- 2번에 설정했던 함수 및 패키지 등이 불러와진다.

**처음 설정시 패키지 설치로 인해 오래 걸림**
