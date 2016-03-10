package data

/**
  * Created by ihji on 3/6/16.
  */
case class Team(leader: UserMonster, member1: UserMonster, member2: UserMonster, member3: UserMonster, member4: UserMonster, friend: UserMonster) {
  def toSeq : Seq[UserMonster] = {
    Seq(leader,member1,member2,member3,member4,friend)
  }
}
