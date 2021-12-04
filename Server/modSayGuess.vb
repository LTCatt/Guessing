Public Module modSayGuess 'SG

#Region "房间信息"

    ''' <summary>
    ''' 你说我猜的房间信息。
    ''' </summary>
    Public Class SGRoomData
        ''' <summary>
        ''' 答案。
        ''' </summary>
        Public Answer As String
        ''' <summary>
        ''' 答案类别。
        ''' </summary>
        Public Hint As String
        ''' <summary>
        ''' 答案选项。
        ''' </summary>
        Public Selections As ArrayList

        ''' <summary>
        ''' 猜对者的分数库。
        ''' </summary>
        Public ScoreBase As Integer
        ''' <summary>
        ''' 猜对者的人数。
        ''' </summary>
        Public CorrectCount As Integer = 0
        ''' <summary>
        ''' 猜对者或使用了一次完成失败的人数。
        ''' </summary>
        Public FinishedCount As Integer = 0
        ''' <summary>
        ''' 一轮结束后的等待。
        ''' </summary>
        Public EndWait As Boolean = False
        ''' <summary>
        ''' 正在描述的人。
        ''' </summary>
        Public Turner As formMain.UserData
        ''' <summary>
        ''' 倒计时线程。
        ''' </summary>
        Public TimerThread As Thread
        ''' <summary>
        ''' 倒计时。
        ''' </summary>
        Public Timer As Integer = 99
        ''' <summary>
        ''' 已显示的提示。
        ''' </summary>
        Public Hinted As New List(Of String)
    End Class

#End Region

#Region "玩家信息"

    ''' <summary>
    ''' 你说我猜的玩家信息。
    ''' </summary>
    Public Class SGUserData
        ''' <summary>
        ''' 是否已经轮过了。
        ''' </summary>
        Public Turned As Boolean = False
        ''' <summary>
        ''' 玩家的游戏状态。
        ''' </summary>
        Public State As SGStates = SGStates.Failed
        ''' <summary>
        ''' 玩家的得分。
        ''' </summary>
        Public Score As Integer = 0
    End Class
    ''' <summary>
    ''' 玩家的游戏中状态。
    ''' </summary>
    Public Enum SGStates As Integer
        ''' <summary>
        ''' 没有猜出来。
        ''' </summary>
        Failed
        ''' <summary>
        ''' 已经猜出来了。
        ''' </summary>
        Finished
        ''' <summary>
        ''' 正在描述。
        ''' </summary>
        Turning
        ''' <summary>
        ''' 旁观。
        ''' </summary>
        Observe
    End Enum
    ''' <summary>
    ''' 获取玩家的显示信息。
    ''' </summary>
    Public Function SGList(room As formMain.RoomData)
        SGList = ""
        '将描述者置顶显示
        For Each u As formMain.UserData In room.Users
            If u.SG.State = SGStates.Turning Then SGList += "Blue/" & u.Name & "/" & u.SG.Score & "/" & u.SG.Turned & "|"
        Next
        '显示其他玩家
        For Each u As formMain.UserData In room.Users
            Select Case u.SG.State
                Case SGStates.Failed
                    SGList += "Red/" & u.Name & "/" & u.SG.Score & "/" & u.SG.Turned & "|"
                Case SGStates.Finished
                    SGList += "Green/" & u.Name & "/" & u.SG.Score & "/" & u.SG.Turned & "|"
            End Select
        Next
    End Function
    ''' <summary>
    ''' 中途加入的旁观玩家。
    ''' </summary>
    Public Sub SGObserve(user As formMain.UserData)
        user.Send("Clear¨Game|你说我猜（旁观）¨" &
                           "Timer|" & user.Room.SG.Timer & "|" & If(user.Room.SG.EndWait, "Blue", If(user.Room.SG.CorrectCount = 0, "Orange", "Red")) & "¨" &
                           "Chatable|" & user.Room.SG.EndWait.ToString & "¨Sureable|False¨" &
                           "SelectClear¨" &
                           "Content|" & If(user.Room.SG.Answer = "",
                                    If(user.Room.SG.EndWait, "描述者未选择题目", "等待描述者选择题目"),
                                    If(user.Room.SG.EndWait, "题目：" & user.Room.SG.Answer, "正在旁观中")
                               ) & "¨" &
                           "Chat|游戏规则：" & vbCrLf &
                                "　一位玩家描述题目，其余玩家进行猜测。" & vbCrLf &
                                "　猜对的玩家越多，描述者得分越高；然而，如果所有玩家全部猜对，描述者不得分。|False")
        frmMain.BoardcastInRoom("Chat|系统：" & user.Name & " 以旁观者身份加入了房间。|False", user.Room)
        user.SG = New SGUserData With {.State = SGStates.Observe, .Turned = True}
    End Sub

#End Region

#Region "题目"

    ''' <summary>
    ''' 题库。（类别，题目）
    ''' </summary>
    Private SGData As New Dictionary(Of String, ArrayList)
    ''' <summary>
    ''' 加载题库。
    ''' </summary>
    Public Sub SGLoad()
        SGData.Add("名词", RandomChaos({"白内障", "红楼梦", "听诊器", "世界杯", "红绿灯", "泥石流", "强迫症", "花生油", "沙尘暴", "龙卷风", "芝麻油", "兵马俑", "文件夹", "原子弹", "浏览器", "康师傅", "玫瑰花", "造纸术", "儿童节", "端午节", "教师节", "国庆节", "元旦", "孙子", "排球", "夜宵", "婴儿", "火锅", "雷暴", "孜然", "矿石", "鳞甲", "图腾", "网易", "珊瑚", "篝火", "火药", "玻璃", "矿车", "腾讯", "表格", "魔术", "尾巴", "股票", "晴天", "台风", "矮人", "魔术", "兽人", "暴雨", "天使", "月亮", "夜宵", "太阳", "地震", "魔法", "银河", "流星", "微信", "恒星", "卫星", "大腿", "心脏", "酱油", "芝麻", "核酸", "病毒", "蒜", "姜", "肝", "肾", "胃", "茶", "肉", "葱", "脚", "神", "鬼", "冰", "雾", "雨", "雪", "火"}))
        SGData.Add("行为", RandomChaos({"系鞋带", "打喷嚏", "刮胡子", "测血压", "查水表", "送快递", "关电脑", "玩游戏", "放风筝", "卖保险", "数星星", "找工作", "抄作业", "写小说", "逛街", "减肥", "跳高", "打滚", "卖萌", "梦游", "挂机", "摸鱼", "注册", "跳跃", "吃鱼", "洗脸", "爬山", "绑架", "植树", "跳楼", "消毒", "考试", "投票", "签字", "领奖", "下载", "报警", "喘气", "逃跑", "学习", "砍树", "洗头", "打字", "作曲", "瞄准", "弯腰", "作弊", "上班", "演讲", "拔河", "穿越", "冷笑", "放屁", "下棋", "出国", "开车", "登录", "洗手", "占卜", "骑马", "灭火", "喝酒", "射击", "喝水", "吃面", "咳嗽", "杀人"}))
        SGData.Add("成语", RandomChaos({"一分为二", "一刀两断", "一石二鸟", "一叶知秋", "不堪一击", "可见一斑", "两肋插刀", "三心二意", "三顾茅庐", "朝三暮四", "入木三分", "雨过天晴", "五花大绑", "九死一生", "万箭穿心", "望子成龙", "蜻蜓点水", "惊弓之鸟", "鹤立鸡群", "守株待兔", "呼风唤雨", "旗鼓相当", "狗急跳墙", "东张西望", "狼吞虎咽", "抱头鼠窜", "龙飞凤舞", "拔刀相助", "坐井观天", "张牙舞爪", "指鹿为马", "虎背熊腰", "草船借箭", "开门见山", "移花接木", "天罗地网", "目无全牛", "兴高采烈", "亡羊补牢", "鸡犬不宁", "望梅止渴", "日积月累", "胆小如鼠", "呆若木鸡", "锦上添花", "叶公好龙", "汗流浃背", "抓耳挠腮", "遍体鳞伤", "立竿见影", "绞尽脑汁", "鸡飞狗跳", "幸灾乐祸", "挥汗如雨", "谈笑风生", "点石成金", "惊弓之鸟", "水滴石穿", "破涕为笑", "手舞足蹈", "回头是岸", "愚公移山", "掩耳盗铃", "眉飞色舞", "哭笑不得", "藕断丝连", "纸上谈兵", "眉开眼笑", "血盆大口", "掌上明珠", "出生入死"}))
        SGData.Add("生物", RandomChaos({"丹顶鹤", "猫头鹰", "长颈鹿", "外星人", "地狱犬", "三文鱼", "美杜莎", "皮卡丘", "穿山甲", "美人鱼", "史莱姆", "独角兽", "萤火虫", "萤火虫", "北极熊", "九尾狐", "宝箱怪", "骆驼", "巨人", "蝌蚪", "青蛙", "蚂蚁", "蜘蛛", "孔雀", "螃蟹", "海马", "母鸡", "蝎子", "鸽子", "海豚", "精灵", "天鹅", "恐龙", "斑马", "鲤鱼", "麻雀", "狼人", "蚊子", "蝙蝠", "章鱼", "蜜蜂", "绵羊", "河豚", "恐龙", "狐狸", "地鼠", "苍蝇", "鳄鱼", "田螺", "水牛", "燕子", "麒麟", "蜥蜴", "狐狸", "鹦鹉", "鲨鱼", "河马", "蜻蜓", "凤凰", "企鹅", "猴", "驴", "鹰", "鹿", "蛇", "虾", "蟹", "蝉", "蚕", "狼", "兔", "龙", "鲸", "熊", "狗", "猫"}))
        SGData.Add("食品", RandomChaos({"三明治", "爆米花", "棉花糖", "烤红薯", "水煮鱼", "柠檬茶", "番茄酱", "茶叶蛋", "无花果", "方便面", "蛋炒饭", "矿泉水", "葡萄干", "干拌面", "薄荷糖", "猕猴桃", "葡萄酒", "八宝粥", "千层肚", "火龙果", "青苹果", "小龙虾", "金针菇", "哈密瓜", "饼干", "香肠", "果醋", "豆浆", "菠萝", "鸭肠", "肥肠", "腰果", "冰糖", "红糖", "南瓜", "辣椒", "酸奶", "菠菜", "小米", "海带", "腰果", "牛排", "紫薯", "咸鱼", "豌豆", "玉米", "奶酪", "咖啡", "木耳", "黄瓜", "桑葚", "甘蔗", "茄子", "榴莲", "血旺", "柚子", "椰子", "雪碧", "山楂", "柠檬", "白酒", "蛋糕", "麻花", "糯米", "馒头", "披萨", "烧饼", "瓜子", "油条", "牛奶", "可乐"}))
        SGData.Add("物品", RandomChaos({"回形针", "三轮车", "指甲油", "手电筒", "飞机票", "绿帽子", "橡皮擦", "红领巾", "不倒翁", "灭火器", "收音机", "笔记本", "服务器", "电子琴", "游戏机", "剃须刀", "榨汁机", "路由器", "电热毯", "三角板", "荧光笔", "三叉戟", "荧光棒", "温度计", "卫生纸", "挖掘机", "充电器", "机关枪", "梳子", "台灯", "铅球", "水晶", "戒指", "花盆", "卷轴", "存折", "锤子", "冰块", "镰刀", "雨衣", "子弹", "蜡烛", "筷子", "电池", "狼皮", "头盔", "秒表", "菜刀", "羊毛", "毛笔", "香皂", "口罩", "金币", "烟花", "木鱼", "口琴", "空调", "枕头", "窗帘", "古筝", "粉笔", "匕首", "铁剑", "石斧", "灯笼", "牙膏", "快板", "锄头", "水壶", "口红", "圆规", "铁锤", "药水", "字典", "法杖", "香水", "剪刀", "彩票"}))
        SGData.Add("地点", RandomChaos({"黑龙江", "天安门", "攀枝花", "石家庄", "葡萄牙", "俄罗斯", "公交站", "游乐园", "停车场", "教学楼", "小卖部", "立交桥", "加油站", "金字塔", "异世界", "土耳其", "水立方", "针叶林", "足球场", "少林寺", "操场", "宿舍", "长城", "鸟巢", "公园", "仓库", "青岛", "山脉", "宫殿", "食堂", "沙滩", "瀑布", "冰山", "盆地", "寺庙", "北京", "教堂", "铁路", "上海", "中国", "美国", "日本", "银行", "花园", "医院", "法院", "酒吧", "墓地", "故宫", "白宫", "英国", "台湾", "厕所", "山东", "阴间", "四川", "长江", "天堂", "巴西", "地狱", "黄河", "澡堂", "网吧", "长沙", "三峡"}))
        SGData.Add("人物", RandomChaos({"周杰伦", "法老", "贾宝玉", "红太狼", "光头强", "派大星", "米老鼠", "钢铁侠", "蜘蛛侠", "赵本山", "林黛玉", "特斯拉", "贝多芬", "毕福剑", "崔永元", "灰太狼", "马化腾", "阿童木", "刘德华", "匹诺曹", "周树人", "特朗普", "王尼玛", "葫芦娃", "薛定谔", "武则天", "孙悟空", "蝙蝠侠", "暖羊羊", "孔乙己", "乔布斯", "司马光", "蓝精灵", "陈独秀", "蟹老板", "刘慈欣", "爱迪生", "奥特曼", "丘比特", "诸葛亮", "牛顿", "闰土", "林冲", "李白", "熊大", "路飞", "姚明", "柯南", "龙猫", "成龙", "耶稣", "刘翔", "武松", "韩红", "贞子", "霍金", "马云", "孔子", "关羽", "唐僧", "曹操"}))
        SGData.Add("MC 方块", RandomChaos({"黑曜石", "侦测器", "黑曜石", "告示牌", "木门", "按钮", "栅栏门", "树叶", "栅栏", "压力板", "树苗", "木板", "楼梯", "活板门", "远古残骸", "安山岩", "铁砧", "旗帜", "木桶", "屏障", "玄武岩", "信标", "基岩", "蜂巢", "蜂箱", "甜菜", "钟", "黑石", "高炉", "煤炭块", "铜块", "钻石块", "绿宝石块", "金块", "铁块", "床", "青金石块", "下界合金块", "石英块", "红石块", "蓝冰", "骨块", "珊瑚", "酿造台", "棕色蘑菇", "气泡柱", "仙人掌", "蛋糕", "营火", "地毯", "胡萝卜", "炼药锅", "锁链", "箱子", "命令方块", "紫颂花", "煤矿石", "圆石", "蜘蛛网", "可可果", "堆肥桶", "潮涌核心", "铜矿石", "工作台", "哭泣的黑曜石", "蒲公英", "钻石矿石", "闪长岩", "发射器", "泥土", "龙蛋", "龙首", "投掷器", "染料", "染色羊毛", "绿宝石矿石", "末地烛", "末地石", "末地石砖", "末影箱", "附魔台", "耕地", "蕨", "火", "制箭台", "花盆", "熔炉", "镶金黑石", "玻璃", "玻璃板", "荧石", "金矿石", "花岗岩", "草", "草方块", "沙砾", "干草块", "蜂蜜块", "漏斗", "铁栏杆", "铁门", "铁矿石", "南瓜灯", "唱片机", "青金石矿石", "避雷针", "睡莲", "磁石", "织布机", "岩浆块", "西瓜", "苔石", "讲台", "菌丝", "下界砖块", "下界金矿石", "下界传送门", "下界石英矿石", "下界疣", "下界岩", "音符盒", "浮冰", "活塞", "灰化土", "细雪", "海晶石", "南瓜", "铁轨", "红色蘑菇", "红沙", "红石灯", "红石比较器", "红石火把", "红石中继器", "重生锚", "沙子", "海晶灯", "海泡菜", "潜影盒", "脚手架", "锻造台", "烟熏炉", "雪", "灵魂灯笼", "灵魂沙", "灵魂火把", "刷怪笼", "海绵", "云杉原木", "石头", "石砖", "切石机", "向日葵", "甘蔗", "高草丛", "火把", "陷阱箱", "绊线钩", "郁金香", "海龟蛋", "藤蔓", "湿海绵", "小麦", "凋零玫瑰", "羊毛", "水"}))
        SGData.Add("MC 物品", RandomChaos({"画", "运输矿车", "动力矿车", "盔甲架", "船", "苹果", "箭", "粗制的药水", "药水", "竹子", "甜菜根", "甜菜种子", "甜菜汤", "烈焰棒", "烈焰粉", "骨头", "骨粉", "书", "附魔之瓶", "弓", "面包", "碗", "红砖", "桶", "河豚桶", "鲑鱼桶", "鳕鱼桶", "热带鱼桶", "胡萝卜钓竿", "藏宝图", "蜡烛", "锁链靴子", "锁链胸甲", "锁链头盔", "锁链护腿", "木炭", "紫颂果", "时钟", "黏土球", "小丑鱼", "煤炭", "可可豆", "指南针", "牛排", "熟鸡肉", "熟鳕鱼", "熟羊肉", "熟猪排", "熟兔肉", "熟鲑鱼", "曲奇", "铜锭", "弩", "钻石", "钻石斧", "钻石胸甲", "钻石靴子", "钻石头盔", "钻石锄", "钻石马铠", "钻石护腿", "钻石镐", "钻石锹", "钻石剑", "龙息", "干海带", "鸡蛋", "绿宝石", "鞘翅", "空地图", "附魔书", "附魔金苹果", "末影水晶", "末影之眼", "末影珍珠", "羽毛", "发酵蛛眼", "地图", "火焰弹", "烟花火箭", "钓鱼竿", "燧石", "打火石", "恶魂之泪", "玻璃瓶", "荧石粉", "金锭", "金粒", "金苹果", "金斧", "金靴子", "金头盔", "金锄", "金胡萝卜", "金胸甲", "金护腿", "金马铠", "金镐", "金锹", "金剑", "火药", "海洋之心", "蜂蜜瓶", "墨囊", "铁靴子", "铁头盔", "铁胸甲", "铁马铠", "铁锭", "铁护腿", "铁斧", "铁锄", "铁粒", "铁镐", "铁锹", "铁剑", "物品展示框", "海带", "皮革", "皮革靴子", "皮革帽子", "皮革马铠", "皮革裤子", "皮革外套", "滞留药水", "蘑菇煲", "音乐唱片", "命名牌", "鹦鹉螺壳", "下界砖", "下界石英", "下界之星", "下界合金斧", "下界合金靴子", "下界合金胸甲", "下界合金头盔", "下界合金锄", "海晶碎片", "河豚", "南瓜派", "南瓜种子", "兔子皮", "兔肉煲", "兔子脚", "生牛肉", "生鸡肉", "生鳕鱼", "生羊肉", "生猪排", "生兔肉", "生鲑鱼", "红石粉", "腐肉", "鞍", "种子", "剪刀", "盾牌", "潜影壳", "刷怪蛋", "雪球", "光灵箭", "蜘蛛眼", "喷溅药水", "水瓶", "望远镜", "木棍", "石斧", "石锄", "石镐", "石锹", "石剑", "线", "糖", "迷之炖菜", "甜浆果", "不死图腾", "三叉戟", "热带鱼", "海龟壳", "水桶", "小麦种子", "木斧", "木锄", "木镐", "木锹", "木剑"}))
        SGData.Add("MC 生物", RandomChaos({"蝙蝠", "烈焰人", "蜜蜂", "洞穴蜘蛛", "闪电苦力怕", "鳕鱼", "苦力怕", "海豚", "溺尸", "远古守卫者", "末影龙", "末影人", "末影螨", "唤魔者", "狐狸", "恶魂", "发光鱿鱼", "山羊", "守卫者", "疣猪兽", "尸壳", "幻术师", "灾厄队长", "铁傀儡", "杀手兔", "羊驼", "岩浆怪", "哞菇", "豹猫", "熊猫", "鹦鹉", "幻翼", "猪灵", "猪灵蛮兵", "掠夺者", "北极熊", "兔子", "劫掠兽", "潜影贝", "绵羊", "鲑鱼", "史莱姆", "骷髅", "蠹虫", "雪傀儡", "蜘蛛", "蜘蛛骑士", "鱿鱼", "流浪者", "炽足兽", "海龟", "恼鬼", "村民", "卫道士", "女巫"}))
    End Sub
    ''' <summary>
    ''' 获取题目。
    ''' </summary>
    Private Function SGGet() As String()
        Dim Type As String = RandomOne({"名词", "行为", "成语", "生物", "食品", "物品", "地点", "人物", "MC 方块", "MC 物品", "MC 生物"})
        '取出该类别第一个题目，再放到靠后的随机位置
        Dim Ques As String = SGData(Type)(0)
        SGData(Type).RemoveAt(0)
        SGData(Type).Insert(RandomInteger(SGData(Type).Count - 20, SGData(Type).Count - 1), Ques)
        Return {Ques, Type}
    End Function

#End Region

#Region "游戏状态改变"

    ''' <summary>
    ''' 开始游戏。
    ''' </summary>
    Public Sub SGStart(room As formMain.RoomData)
        '发布公告
        frmMain.BoardcastInRoom("Clear¨Game|你说我猜¨" &
                                "Chat|游戏规则：" & vbCrLf &
                                "　一位玩家描述题目，其余玩家进行猜测。" & vbCrLf &
                                "　猜对的玩家越多，描述者得分越高；然而，如果所有玩家全部猜对，描述者不得分。|False", room)
        '初始化数据
        room.SG = New SGRoomData
        For Each pc As formMain.UserData In room.Users
            pc.SG = New SGUserData
        Next
        room.SG.TimerThread = New Thread(AddressOf SGTimer)
        room.SG.TimerThread.Start(room)
        '随机一人开始描述
        room.SG.Turner = RandomOne(room.Users)
        SGTurnStart(room)
    End Sub
    ''' <summary>
    ''' 结束游戏。
    ''' </summary>
    Public Sub SGEnd(room As formMain.RoomData)
        Dim chats As New ArrayList
        Dim players As New ArrayList
        Dim rank As Integer = 1 '排名
        players.AddRange(room.GetGameUsers)
        Do Until players.Count = 0
            '查找最佳玩家列表
            Dim bp As Integer = -1
            Dim bps As New ArrayList
            For Each player As formMain.UserData In players
                If player.SG.Score > bp Then
                    bp = player.SG.Score
                    bps.Clear()
                    bps.Add(player.Name)
                ElseIf player.SG.Score = bp Then
                    bps.Add(player.Name)
                End If
            Next
            '显示分数
            chats.Add("第 " & rank & " 名：" & Join(bps.ToArray, "、") & "（" & bp & " 分）")
            rank += bps.Count
            '移除这些玩家
            For Each name As String In bps
                For i = 0 To players.Count - 1
                    If players(i).Name = name Then
                        players.RemoveAt(i)
                        GoTo NextPlayer
                    End If
                Next
NextPlayer:
            Next
        Loop
        chats(0) = "Chat|" & chats(0)
        frmMain.BoardcastInRoom("Chat|系统：游戏结束！|False¨" & Join(chats.ToArray, vbCrLf) & "|True", room)
    End Sub
    ''' <summary>
    ''' 玩家中途离开游戏。
    ''' </summary>
    Public Sub SGLeave(user As formMain.UserData, room As formMain.RoomData)
        If user.SG.State = SGStates.Turning And Not room.SG.EndWait Then
            '正在描述的玩家突然溜走
            SGTurnEnd(room)
        ElseIf room.SG.EndWait Then
            '正在等待，重新选人
            Dim unturned As New ArrayList
            For Each u As formMain.UserData In room.GetGameUsers
                If Not u.SG.Turned Then unturned.Add(u)
            Next
            If unturned.Count = 0 Then
                '结束游戏
                room.SG.Turner = Nothing
            Else
                '进入等待
                room.SG.Turner = RandomOne(unturned)
            End If
            room.SG.EndWait = True
        ElseIf user.SG.State = SGStates.Failed Then
            '没有猜出来的玩家退出，重新进行结束判断
            If room.SG.FinishedCount + 1 = room.GetGameUsers.Count Then SGTurnEnd(room)
        End If
    End Sub

#End Region

    ''' <summary>
    ''' 接管聊天。
    ''' </summary>
    Public Sub SGChat(data As String, believe As Boolean, u As formMain.UserData)
        If u.SG.State = SGStates.Failed And Not u.Room.SG.EndWait Then
            If data = u.Room.SG.Answer And Not u.Room.SG.Answer = "" Then
                SGCorrect(u, believe)
                u.Send("Sureable|False")
            Else
                frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|" & If(u.SG.State = SGStates.Turning, "True", "False"), u.Room)
                If believe Then
                    Dim RemoveScore As Integer = Math.Ceiling(u.SG.Score / 2)
                    If RemoveScore = 0 Then
                        frmMain.BoardcastInRoom("Chat|系统：" & u.Name & " 的超勇尝试失败。|False", u.Room)
                    Else
                        u.SG.Score -= RemoveScore
                        frmMain.BoardcastInRoom("Chat|系统：" & u.Name & " 的超勇尝试失败，扣 " & RemoveScore & " 分！|False¨" &
                                                          frmMain.BoardcastList(u.Room), u.Room)
                    End If
                    u.Send("Sureable|False")
                End If
            End If
        Else
            frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|" & If(u.SG.State = SGStates.Turning, "True", "False"), u.Room)
        End If

        ''正在等待中
        'If u.Room.SG.EndWait Then
        '    frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|False", u.Room)
        '    Exit Sub
        'End If
        ''不在等待中
        'Select Case u.SG.State
        '    Case SGStates.Turning
        '        '该玩家正在描述
        '        'For i As Integer = 0 To u.Room.SG.Answer.Length - 1
        '        '    data = data.Replace(u.Room.SG.Answer.Substring(i, 1), "*")
        '        'Next
        '        'If Not data = "" Then frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|True", u.Room)
        '        frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|True", u.Room)
        '    Case SGStates.Failed
        '        '该玩家正在猜测
        '        If data = u.Room.SG.Answer Then
        '            SGCorrect(u)
        '        Else
        '            frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|False", u.Room)
        '        End If
        '    Case SGStates.Finished
        '        '该玩家正在看戏
        '        'For i As Integer = 0 To u.Room.SG.Answer.Length - 1
        '        '    data = data.Replace(u.Room.SG.Answer.Substring(i, 1), "*")
        '        'Next
        '        'If Not data = "" Then frmMain.BoardcastInRoom("Chat|" & u.Name & "：" & data & "|False", u.Room)
        'End Select
    End Sub

    ''' <summary>
    ''' 某人答对了。
    ''' </summary>
    Private Sub SGCorrect(u As formMain.UserData, believe As Boolean)
        '改变玩家数据
        u.SG.Score += u.Room.SG.ScoreBase
        If believe Then u.SG.Score += 2
        u.SG.State = SGStates.Finished
        u.Room.SG.CorrectCount += 1
        u.Room.SG.FinishedCount += 1
        frmMain.BoardcastInRoom("Chat|系统：" & u.Name & " 猜出了答案，得 " & (u.Room.SG.ScoreBase + If(believe, 2, 0)) & " 分" & If(believe, "（超勇尝试成功 +2）", "") & "。" & "|False¨" &
                                                          frmMain.BoardcastList(u.Room), u.Room)
        u.Room.SG.ScoreBase -= 1
        '快速结束
        If u.Room.SG.CorrectCount = 1 Then
            frmMain.BoardcastInRoom("Chat|系统：有玩家已猜出答案，本轮将在 30 秒后结束！|False¨Timer|30|Red", u.Room)
            u.Room.SG.Timer = 30
        End If
        '本轮结束
        If u.Room.SG.FinishedCount + 1 = u.Room.GetGameUsers.Count Then
            SGTurnEnd(u.Room)
        Else
            u.Send("Chatable|False¨Sureable|False")
        End If
    End Sub

    ''' <summary>
    ''' 描述开始。
    ''' </summary>
    Private Sub SGTurnStart(room As formMain.RoomData)
        Dim player As formMain.UserData = room.SG.Turner
        '初始化玩家状态
        For Each pc As formMain.UserData In room.GetGameUsers
            pc.SG.State = SGStates.Failed
        Next
        '对描述者进行标记
        player.SG.State = SGStates.Turning
        player.SG.Turned = True
        room.SG.Turner = player
        '更新状态
        frmMain.BoardcastInRoom(frmMain.BoardcastList(room), room)
        room.SG.ScoreBase = room.GetGameUsers.Count - 1
        room.SG.CorrectCount = 0 : room.SG.FinishedCount = 0
        room.SG.EndWait = False
        room.SG.Hinted = New List(Of String)
        '获取题目
        room.SG.Selections = New ArrayList From {SGGet(), SGGet()}
        room.SG.Answer = ""
        room.SG.Hint = ""
        '发放状态
        For Each us As formMain.UserData In room.Users
            Select Case us.SG.State
                Case SGStates.Turning
                    us.Send("Chat|系统：由你进行描述，请选择一个题目。|True¨" &
                                   "Content|请从下方两个题目中选择一个¨" &
                                   "Select|" & room.SG.Selections(0)(0) & "（" & room.SG.Selections(0)(1) & "）|" & room.SG.Selections(1)(0) & "（" & room.SG.Selections(1)(1) & "）¨" &
                                   "Timer|99|Orange¨" &
                                   "Chatable|False¨Sureable|False")
                Case SGStates.Observe
                    us.Send("Chat|系统：由 " & player.Name & " 进行描述。|True¨" &
                                   "Content|等待描述者选择题目¨" &
                                   "SelectClear¨" &
                                   "Timer|99|Orange¨" &
                                   "Chatable|False¨Sureable|False")
                Case SGStates.Failed
                    us.Send("Chat|系统：由 " & player.Name & " 进行描述。|True¨" &
                                   "Content|等待描述者选择题目¨" &
                                   "SelectClear¨" &
                                   "Timer|99|Orange¨" &
                                   "Chatable|True¨Sureable|True")
            End Select
        Next
        '倒计时
        room.SG.Timer = 99
    End Sub
    ''' <summary>
    ''' 描述结束。
    ''' </summary>
    Private Sub SGTurnEnd(room As formMain.RoomData)
        '加分
        If room.SG.Answer = "" Then
            frmMain.BoardcastInRoom("Content|描述者未选择题目¨" &
                                                              "Chat|系统：描述者 " & room.SG.Turner.Name & " 未选择题目，本轮结束。|True¨" &
                                                              "Chatable|True¨Sureable|False¨" &
                                                              "SelectClear", room)
            room.SG.Timer = 5
        Else
            frmMain.BoardcastInRoom("Content|题目：" & room.SG.Answer, room)
            Dim chat As String = "Chat|系统：本轮题目为【" & room.SG.Answer & "】，"
            If Not room.Users.Contains(room.SG.Turner) Then
                chat += "由于描述者 " & room.SG.Turner.Name & " 离线，本轮直接结束。"
                room.SG.Timer = 5
            ElseIf room.SG.CorrectCount + 1 = room.GetGameUsers.Count Then
                chat += "所有玩家均答对，描述者 " & room.SG.Turner.Name & " 不得分。"
                room.SG.Timer = 5
            ElseIf room.SG.CorrectCount = 0 Then
                chat += "无人答对，描述者 " & room.SG.Turner.Name & " 不得分。"
                room.SG.Timer = 15
            Else
                Dim sc As Integer = room.SG.CorrectCount * 2
                chat += "有 " & room.SG.CorrectCount & " 人答对，描述者 " & room.SG.Turner.Name & " 得 " & sc & " 分。"
                room.SG.Timer = 15
                room.SG.Turner.SG.Score += sc
                frmMain.BoardcastInRoom(frmMain.BoardcastList(room), room)
            End If
            frmMain.BoardcastInRoom(chat & "|True¨Chatable|True¨Sureable|False¨SelectClear", room)
        End If
        '设置尚未描述的玩家
        Dim unturned As New ArrayList
        For Each u As formMain.UserData In room.GetGameUsers
            If Not u.SG.Turned Then unturned.Add(u)
        Next
        If unturned.Count = 0 Then
            '在倒计时后结束游戏
            room.SG.Turner = Nothing
            room.SG.Timer = 5
        Else
            '进入等待
            room.SG.Turner = RandomOne(unturned)
        End If
        room.SG.EndWait = True
        frmMain.BoardcastInRoom("Timer|" & room.SG.Timer & "|Blue", room)
    End Sub

    Private Sub SGTimer(room As formMain.RoomData)
        Do While room.Gaming And frmMain.RoomList.Contains(room)
            room.SG.Timer -= 1
            If room.SG.Timer = 0 Then
                If room.SG.EndWait Then
                    If IsNothing(room.SG.Turner) Then
                        room.EndGame()
                        Exit Sub
                    Else
                        SGTurnStart(room)
                    End If
                Else
                    SGTurnEnd(room)
                End If
            End If
            If room.SG.Timer = 69 And room.SG.Answer = "" Then
                '没有选择题目，强制结束
                SGTurnEnd(room)
            End If
            Thread.Sleep(999)
        Loop
    End Sub

End Module
