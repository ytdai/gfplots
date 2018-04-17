setClass("Hero",
         slots = c(HP = "numeric",
                      MP = "numeric",
                      HRR = "numeric",
                      MRR = "numeric",
                      AD = "numeric",
                      AP = "numeric",
                      ATKspeed = "numeric",
                      Armor = "numeric",
                      MR = "numeric",
                      SPEED = "numeric",
                      SkillCD = "numeric",
                      SkillCoolTime = "numeric"),
         prototype =c(HP = 0, MP = 0,
                         HRR = 0, MRR = 0,
                         AD = 0, AP = 0,
                         ATKspeed = 0,
                         SPEED = 0,
                         Armor = 0, MR = 0,
                         SkillCD = 0,
                         SkillCoolTime = 0))

## 对变量检验进行设置,例如魔法值不能低于0
setValidity("Hero",
            function(object){
              if(object@MP < 0){
                stop("MP 不能低于0")
              }
            }
)

# 1.定义接口：普通攻击
setGeneric("CommonAttack",
           function(obj1, obj2, ...){ # 普通攻击
             standardGeneric("CommonAttack")
           }
)
setGeneric("UpDateCd",
           function(obj1, ...){ # 普通
             standardGeneric("UpDateCd")
           }
)
# 2.定义默认函数（即面对父类Hero的计算方式）
# 2.1 当泛型函数针对父类和子类，当子类没有定义时，便会调用父类的计算方式。
setMethod("CommonAttack", "Hero",
          function(obj1, obj2, cat = T){
            hurt <- obj1@AD - obj2@Armor # A对B造成的伤害 = A的攻击力-B的护甲
            if(cat){
              cat(paste(obj1@name , "Use CommonAttack", obj2@name, "HP: -", hurt), "\n")
            }
            obj2@HP <- obj2@HP - hurt
            return(obj2)
          }
)

setMethod("UpDateCd", "Hero",
          function(obj1, tStep) {
            obj1@SkillCoolTime <- 0
            return(obj1)
          })

### 申明人物A，继承父类Hero
setClass("A", contains = "Hero", slots = list(name = "character"))
# 测试样本1 # 实例化A：基础属性
A <- new("A",
         name = "a",
         HP = 550,
         MP = 340,
         HRR = 1.8,
         MRR = 12,
         AD = 55 ,
         AP = 25,
         ATKspeed = 0.63,
         Armor = 25,
         MR = 30,
         SPEED = 400,
         SkillCD = 1/0.63,
         SkillCoolTime = 0)
# 定义A的普通攻击计算方式：
setMethod("CommonAttack", "A",
          function(obj1, obj2, cat=T){ # A特殊伤害公式
            hurt <- obj1@AD*0.5 + obj1@AP*0.5 - obj2@Armor*0.8 #随便乱写
            if(cat){ # 是否再调用时输出文字描述
              cat(paste(obj1@name , "Use CommonAttack : ", obj2@name, "HP: -", hurt), "\n")
            }
            obj2@HP <- obj2@HP - hurt
            return(obj2)
          }
)
### 申明人物B，继承父类Hero
setClass("B", contains = "Hero", slots = list(name = "character")) # 测试样本2
B <- new("B",
         name = "b",
         HP = 550,
         MP = 0,
         HRR = 1.8,
         MRR = 12,
         AD = 58 ,
         AP = 0,
         ATKspeed = 0.70,
         Armor = 25,
         MR = 20,
         SPEED = 400,
         SkillCD = 1/0.70,
         SkillCoolTime = 0)
# B采用默认的攻击方式，故不进行设定

t <- 0 # 时间初始计数
tStep <- 0.1 # 时间计数间隔
# 平
while(A@HP > 0 & B@HP > 0){
  if(A@SkillCoolTime == 0){ # 当A普通攻击cd为0，A攻击B
    B <- CommonAttack(A, B)
    A@SkillCoolTime <- A@SkillCD
  }
  if(B@SkillCoolTime == 0){ # 当B普通攻击cd为0，B攻击A
    A <- CommonAttack(B, A)
    B@SkillCoolTime <- B@SkillCD
  }
  # CD更新
  A <- UpDateCd(A, tStep)
  B <- UpDateCd(B, tStep)
  t <- t + tStep # 时间流逝
}


