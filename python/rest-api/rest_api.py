import json

from typing import List, Dict
from dataclasses import dataclass, field


@dataclass
class User:
    name: str
    balance: float = 0.0
    owes: Dict[str, float] = field(default_factory=dict)
    owed_by: Dict[str, float] = field(default_factory=dict)

    def borrow(self, person: str, amount: float):
        self.balance -= amount
        if person in self.owed_by:
            self.owed_by[person] -= amount
            if self.owed_by[person] < 0:
                self.owes[person] = abs(self.owed_by[person])
            if self.owed_by[person] <= 0:
                self.owed_by.pop(person, None)
        else:
            if person in self.owes:
                self.owes[person] += amount
            else:
                self.owes[person] = amount

    def lend(self, person: str, amount: float):
        self.balance += amount
        if person in self.owes:
            self.owes[person] -= amount
            if self.owes[person] < 0:
                self.owed_by[person] = abs(self.owes[person])
            if self.owes[person] <= 0:
                self.owes.pop(person, None)
        else:
            if person in self.owed_by:
                self.owed_by[person] += amount
            else:
                self.owed_by[person] = amount

    def toDict(self):
        return {
            "name": self.name,
            "balance": self.balance,
            "owes": self.owes,
            "owed_by": self.owed_by
        }

    def toJSON(self):
        return json.dumps(self.toDict())

class RestAPI:
    __slots__ = ['database']
    def __init__(self, database: Dict[str, List[User]]={"users": []}):
        self.database = {}
        self.database["users"] = list(map(lambda u: User(**u), database["users"]))

    def get_users(self, users: List[str]):
        userData = filter(lambda u: u.name in users, self.database["users"])
        return list(userData)

    def get(self, url, payload=None):
        if url == "/users":
            if payload is None:
                return json.dumps(self.database)
            else:
                payload = json.loads(payload)
                users = payload["users"]
                userData = list(map(lambda u: u.toDict(), self.get_users(users)))
                return json.dumps({
                    "users": userData
                 })
        return json.dumps({})

    def post(self, url, payload=None):
        payload = json.loads(payload)
        if url == "/add":
            newUser = User(payload["user"])
            self.database["users"] += [newUser]
            return newUser.toJSON()
        if url == "/iou":
            lender = self.get_users(payload["lender"])[0]
            borrower = self.get_users(payload["borrower"])[0]
            lender.lend(payload["borrower"], payload["amount"])
            borrower.borrow(payload["lender"], payload["amount"])
            return json.dumps({
                "users": sorted([
                    borrower.toDict(),
                    lender.toDict()
                ], key=lambda x: x["name"])
            })
        return json.dumps({})
